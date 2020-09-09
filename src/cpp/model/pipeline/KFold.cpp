#include "KFold.h"

#include <algorithm>

#include <gsl/gsl_randist.h>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        KFold::KFold(const EvidenceSet& data,
                     int num_folds,
                     shared_ptr<gsl_rng> random_generator) {

            this->split(data, num_folds, random_generator);
        }

        KFold::KFold(const EvidenceSet& training_data,
                     const EvidenceSet& test_data) {

            this->splits.push_back(make_pair(training_data, test_data));
        }

        KFold::~KFold() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void KFold::split(const EvidenceSet& data,
                          int num_folds,
                          shared_ptr<gsl_rng> random_generator) {

            if (num_folds > data.get_num_data_points()) {
                throw invalid_argument(
                    "The number of folds is bigger than the number of data "
                    "points in the evidence set.");
            }

            vector<int> fold_sizes =
                this->get_fold_sizes(data.get_num_data_points(), num_folds);
            vector<int> shuffled_indices = this->get_shuffled_indices(
                data.get_num_data_points(), random_generator);

            this->splits.reserve(num_folds);
            int start_idx = 0;

            for (int i = 0; i < num_folds; i++) {
                int end_idx = start_idx + fold_sizes[i] - 1;

                EvidenceSet training;
                EvidenceSet test;
                vector<int> training_indices;
                if (start_idx > 0) {
                    training_indices.insert(training_indices.end(),
                                            shuffled_indices.begin(),
                                            shuffled_indices.begin() +
                                                start_idx);
                }
                if (end_idx < shuffled_indices.size() - 1) {
                    training_indices.insert(training_indices.end(),
                                            shuffled_indices.begin() + end_idx +
                                                1,
                                            shuffled_indices.end());
                }
                vector<int> test_indices(
                    shuffled_indices.begin() + start_idx,
                    shuffled_indices.begin() + end_idx + 1);

                for (auto& node_label : data.get_node_labels()) {
                    Tensor3 node_data = data[node_label];
                    Tensor3 training_data =
                        node_data.slice(training_indices, 1);
                    Tensor3 test_data = node_data.slice(test_indices, 1);

                    training.add_data(node_label, training_data);
                    test.add_data(node_label, test_data);
                }

                if (num_folds == 1) {
                    // In this case the single fold must be used as training
                    // data instead of test data.
                    this->splits.push_back(make_pair(test, training));
                }
                else {
                    this->splits.push_back(make_pair(training, test));
                }

                start_idx = end_idx + 1;
            }
        }

        vector<int> KFold::get_shuffled_indices(
            int num_data_points,
            shared_ptr<gsl_rng> random_generator) const {
            int* indices = new int[num_data_points];
            for (int i = 0; i < num_data_points; i++) {
                indices[i] = i;
            }

            gsl_ran_shuffle(
                random_generator.get(), indices, num_data_points, sizeof(int));

            return vector<int>(indices, indices + num_data_points);
        }

        vector<int> KFold::get_fold_sizes(int num_data_points,
                                               int num_folds) const {
            int fold_size = floor(num_data_points / num_folds);
            int excess = num_data_points % num_folds;
            vector<int> fold_sizes(num_folds);

            for (int i = 0; i < num_folds; i++) {
                fold_sizes[i] = fold_size;
                if (excess > 0) {
                    fold_sizes[i]++;
                    excess--;
                }
            }

            return fold_sizes;
        }

        void KFold::get_info(nlohmann::json& json) const {
            json["num_folds"] = this->splits.size();
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        const vector<KFold::Split>& KFold::get_splits() const { return splits; }

    } // namespace model
} // namespace tomcat
