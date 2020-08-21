#include "KFold.h"

#include <algorithm>

#include <gsl/gsl_randist.h>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        KFold::KFold(std::shared_ptr<gsl_rng> random_generator, int num_folds)
            : random_generator(random_generator), num_folds(num_folds) {}

        KFold::~KFold() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        KFold::Split KFold::split(const DBNData& data) {

            if (this->num_folds > data.get_num_data_points()) {
                throw std::invalid_argument(
                    "The number of folds is bigger than the number of data "
                    "points in the evidence set.");
            }

            std::vector<int> fold_sizes =
                this->get_fold_sizes(data.get_num_data_points());
            std::vector<int> shuffled_indices =
                this->get_shuffled_indices(data.get_num_data_points());

            Split splits;
            splits.reserve(this->num_folds);
            int start_idx = 0;

            for (int i = 0; i < this->num_folds; i++) {
                int end_idx = start_idx + fold_sizes[i] - 1;

                DBNData training;
                DBNData test;
                std::vector<int> training_indices;
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
                std::vector<int> test_indices(
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
                    splits.push_back(std::make_pair(test, training));
                }
                else {
                    splits.push_back(std::make_pair(training, test));
                }

                start_idx = end_idx + 1;
            }

            return splits;
        }

        std::vector<int>
        KFold::get_shuffled_indices(int num_data_points) const {
            int* indices = new int[num_data_points];
            for (int i = 0; i < num_data_points; i++) {
                indices[i] = i;
            }

            gsl_ran_shuffle(this->random_generator.get(),
                            indices,
                            num_data_points,
                            sizeof(int));

            return std::vector<int>(indices, indices + num_data_points);
        }

        std::vector<int> KFold::get_fold_sizes(int num_data_points) const {
            int fold_size = floor(num_data_points / this->num_folds);
            int excess = num_data_points % this->num_folds;
            std::vector<int> fold_sizes(this->num_folds);

            for (int i = 0; i < this->num_folds; i++) {
                fold_sizes[i] = fold_size;
                if (excess > 0) {
                    fold_sizes[i]++;
                    excess--;
                }
            }

            return fold_sizes;
        }

    } // namespace model
} // namespace tomcat
