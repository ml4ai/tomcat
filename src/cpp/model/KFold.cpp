#include "KFold.h"

#include <algorithm>

#include <gsl/gsl_randist.h>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        KFold::KFold(int num_folds) : num_folds(num_folds) {}

        KFold::~KFold() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::vector<std::pair<EvidenceSet, EvidenceSet>>
        KFold::split(std::shared_ptr<gsl_rng> random_generator,
                     const EvidenceSet& data) {

            if (this->num_folds > data.get_num_data_points()) {
                throw std::invalid_argument(
                    "The number of folds is bigger than the number of data "
                    "points in the evidence set.");
            }

            std::vector<int> fold_sizes =
                this->get_fold_sizes(data.get_num_data_points());
            std::vector<int> shuffled_indices = this->get_shuffled_indices(
                random_generator, data.get_num_data_points());

            std::vector<std::pair<EvidenceSet, EvidenceSet>> folds;
            folds.reserve(this->num_folds);
            int start_idx = 0;

            for (int i = 0; i < this->num_folds; i++) {
                int end_idx = start_idx + fold_sizes[i] - 1;

                EvidenceSet training;
                EvidenceSet test;
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
                    Tensor3 training_data = node_data.slice(training_indices, 1);
                    Tensor3 test_data = node_data.slice(test_indices, 1);

                    training.add_data(node_label, training_data);
                    test.add_data(node_label, test_data);
                }

                folds.push_back(std::make_pair(training, test));
                start_idx = end_idx + 1;
            }

            return folds;
        }

        std::vector<int>
        KFold::get_shuffled_indices(std::shared_ptr<gsl_rng> random_generator,
                                    int num_data_points) const {
            int* indices = new int[num_data_points];
            for (int i = 0; i < num_data_points; i++) {
                indices[i] = i;
            }

            gsl_ran_shuffle(
                random_generator.get(), indices, num_data_points, sizeof(int));

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

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
