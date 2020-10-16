#include <memory>
#include <string>
#include <vector>

#include <boost/program_options.hpp>
#include <eigen3/Eigen/Dense>
#include <fmt/format.h>
#include <gsl/gsl_rng.h>

#include "converter/TA3MessageConverter.h"
#include "experiments/TomcatTA3.h"
#include "experiments/TomcatTA3V2.h"
#include "pgm/EvidenceSet.h"
#include "pipeline/DBNSaver.h"
#include "pipeline/KFold.h"
#include "pipeline/Pipeline.h"
#include "pipeline/estimation/OnlineEstimation.h"
#include "pipeline/estimation/SumProductEstimator.h"
#include "pipeline/training/DBNLoader.h"

using namespace tomcat::model;
using namespace std;
namespace po = boost::program_options;

void run_tomcat(const string& model_dir,
                const string& map_config_filepath,
                const string& broker_address,
                int broker_port,
                int timeout) {
    // Model
    TomcatTA3V2 tomcat;
    tomcat.init();

    // Data
    EvidenceSet training_data; // Empty (model is pre-trained)
    EvidenceSet test_data;     // Empty (it comes online)

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    // Estimation
    OnlineEstimation::MessageBrokerConfiguration config;
    config.timeout = timeout;
    config.address = broker_address;
    config.port = broker_port;

    shared_ptr<MessageConverter> converter =
        make_shared<TA3MessageConverter>(map_config_filepath);
    shared_ptr<OnlineEstimation> online_estimation =
        make_shared<OnlineEstimation>(config, converter);

    int horizon = 1;
    shared_ptr<Estimator> estimator =
        make_shared<SumProductEstimator>(tomcat.get_model(),
                                         horizon,
                                         TomcatTA3::SG,
                                         Eigen::VectorXd::Constant(1, 1));
    online_estimation->add_estimator(estimator);
    estimator =
        make_shared<SumProductEstimator>(tomcat.get_model(),
                                         horizon,
                                         TomcatTA3::SY,
                                         Eigen::VectorXd::Constant(1, 1));
    online_estimation->add_estimator(estimator);

    Pipeline pipeline;
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(online_estimation);
    pipeline.execute();
}

int main(int argc, char* argv[]) {
    string model_dir;
    string map_config_filepath;
    string broker_address;
    unsigned int broker_port;
    unsigned int timeout;

    po::options_description desc("Allowed options");
    desc.add_options()("help,h", "Produce this help message")(
        "model_dir",
        po::value<string>(&model_dir)
            ->default_value("../../data/model/ta3/asist"),
        "Directory where the parameters of a pre-trained model are stored.")(
        "map_config",
        po::value<string>(&map_config_filepath)
            ->default_value("../../data/maps/ta3/falcon_v1.0.json"),
        "Filepath of the .json file containing the mission map configuration.")(
        "broker_address",
        po::value<string>(&broker_address)->default_value("localhost"),
        "Address of the message broker.\n")(
        "broker_address",
        po::value<unsigned int>(&broker_port)->default_value(1883),
        "Port of the message broker.\n")(
        "timeout",
        po::value<unsigned int>(&timeout)->default_value(20),
        "Number of seconds to wait for a message before closing connection "
        "with the message broker.\n");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help") || !vm.count("experiment_id")) {
        cout << desc << "\n";
        return 1;
    }

    run_tomcat(
        model_dir, map_config_filepath, broker_address, broker_port, timeout);
}
