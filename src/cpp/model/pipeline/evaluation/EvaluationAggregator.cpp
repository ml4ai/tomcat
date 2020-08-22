#include "EvaluationAggregator.h"

#include <sstream>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        EvaluationAggregator::EvaluationAggregator(METHOD method)
            : method(method) {}

        EvaluationAggregator::~EvaluationAggregator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void EvaluationAggregator::reset() {
            for (auto& evaluations : this->evaluations_per_measure) {
                evaluations.clear();
            }

            for (auto& aggregations : this->aggregations_per_measure) {
                aggregations.clear();
            }
        }

        void
        EvaluationAggregator::add_measure(std::shared_ptr<Measure> measure) {
            this->measures.push_back(measure);
            this->evaluations_per_measure.push_back({});
            this->aggregations_per_measure.push_back({});
        }

        void EvaluationAggregator::evaluate(const EvidenceSet& test_data) {
            int i = 0;
            for (auto& measure : this->measures) {
                this->evaluations_per_measure[i].push_back(
                    measure->evaluate(test_data));
                i++;
            }
        }

        void EvaluationAggregator::aggregate() {
            int m = 0;
            for (auto& evaluations : this->evaluations_per_measure) {
                if (!evaluations.empty()) {
                    int num_nodes_evaluated = evaluations[0].size();

                    // The evaluations for a given node and assignment are
                    // stored in the rows of a single column. So we loop in a
                    // row-wise fashion to aggregate the values for a given
                    // node.
                    for (int j = 0; j < num_nodes_evaluated; j++) {
                        std::vector<Tensor3> evaluations_per_node;
                        evaluations_per_node.reserve(evaluations.size());

                        NodeEvaluationAggregation node_aggregation;
                        node_aggregation.label = evaluations[0][j].label;
                        node_aggregation.assignment =
                            evaluations[0][j].assignment;
                        for (int i = 0; i < evaluations.size(); i++) {
                            evaluations_per_node.push_back(
                                evaluations[i][j].evaluation);
                        }
                        node_aggregation.aggregated_evaluation =
                            this->compute_aggregation(evaluations_per_node);
                        this->aggregations_per_measure[m].push_back(
                            node_aggregation);
                    }
                }
                m++;
            }
        }

        Aggregation EvaluationAggregator::compute_aggregation(
            const std::vector<Tensor3>& evaluations) const {

            Aggregation aggregation;

            switch (this->method) {
            case METHOD::no_aggregation: {
                aggregation.aggregated_values = evaluations;
                break;
            }
            case METHOD::average: {
                aggregation.aggregated_values = std::vector<Tensor3>(1);
                aggregation.aggregated_values[0] = Tensor3::mean(evaluations);
                aggregation.errors = Tensor3::std(evaluations);
                break;
            }
            default: {
                throw TomcatModelException(
                    "The aggregation method assigned is not a valid one.");
            }
            }

            return aggregation;
        }

        void EvaluationAggregator::get_info(nlohmann::json& json) const {
            json = nlohmann::json::array();
            for (int m = 0; m < this->measures.size(); m++) {
                nlohmann::json json_aggregation;
                json_aggregation["aggregation_method"] =
                    this->get_method_name();
                this->measures[m]->get_info(json_aggregation["measure"]);
                json_aggregation["results"] = nlohmann::json::array();
                nlohmann::json json_result;

                for (const auto& aggregations_per_node :
                     this->aggregations_per_measure[m]) {
                    json_result["node_label"] = aggregations_per_node.label;

                    // The Eigen::VectorXd class does not have a to_string
                    // function.
                    std::stringstream ss;
                    ss << aggregations_per_node.assignment;
                    json_result["node_assignment"] = ss.str();

                    json_result["aggregated_value"] = nlohmann::json::array();
                    for (const auto& aggregated_value :
                         aggregations_per_node.aggregated_evaluation
                             .aggregated_values) {
                        json_result["aggregated_value"].push_back(
                            aggregated_value.to_string());
                        LOG(aggregated_value);
                    }
                    json_result["error"] =
                        aggregations_per_node.aggregated_evaluation.errors
                            .to_string();
                }
                json_aggregation["results"].push_back(json_result);
                json.push_back(json_aggregation);
            }
        }

        std::string EvaluationAggregator::get_method_name() const {
            std::string method_name = "";

            switch (this->method) {
            case METHOD::no_aggregation: {
                method_name = "no aggregation";
                break;
            }
            case METHOD::average: {
                method_name = "average";
                break;
            }
            }

            return method_name;
        }

    } // namespace model
} // namespace tomcat
