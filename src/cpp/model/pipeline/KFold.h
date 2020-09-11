#pragma once

#include <memory>
#include <utility>
#include <vector>

#include <gsl/gsl_rng.h>
#include <nlohmann/json.hpp>

#include "model/pgm/EvidenceSet.h"
#include "model/utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * This class is responsible for splitting data into disjoint folds
         * where k-1 of them is used for training data and the remaining one for
         * test data creating a list of this pair of data sets by repeating this
         * logic for all the k folds.
         */
        class KFold {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            typedef std::pair<EvidenceSet, EvidenceSet> Split;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a KFold data splitter.
             *
             * @param data: data to be split
             * @param num_folds: number of splits
             * @param random_generator: random number generator
             */
            KFold(const EvidenceSet& data,
                  int num_folds,
                  std::shared_ptr<gsl_rng> random_generator);

            KFold(const EvidenceSet& training_data,
                  const EvidenceSet& test_data);

            ~KFold();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            KFold(const KFold&) = default;

            KFold& operator=(const KFold&) = default;

            KFold(KFold&&) = default;

            KFold& operator=(KFold&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            void get_info(nlohmann::json& json) const;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::vector<Split>& get_splits() const;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Creates k data splits comprised of disjoint training and test
             * set.
             *
             * @param data: data to be split
             * @param num_folds: number of splits
             * @param random_generator: random number generator
             */
            void split(const EvidenceSet& data,
                       int num_folds,
                       std::shared_ptr<gsl_rng> random_generator);

            /**
             * Returns a list of shuffled indices of the data points.
             *
             * @param num_data_points: number of data points that will be
             * split into folds
             * @param random_generator: random number generator
             *
             * @return Shuffled indices of the data points.
             */
            std::vector<int> get_shuffled_indices(
                int num_data_points,
                std::shared_ptr<gsl_rng> random_generator) const;

            /**
             * Returns the number of data points in each fold.
             *
             * @param num_data_points: number of data points that will be
             * split into folds
             * @param num_folds: number of splits
             *
             * @return Number of data points in each one of the folds.
             */
            std::vector<int> get_fold_sizes(int num_data_points, int num_folds) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Data split in training and test sets. Populated in the creation
            // of the class object.
            std::vector<Split> splits;
        };

    } // namespace model
} // namespace tomcat
