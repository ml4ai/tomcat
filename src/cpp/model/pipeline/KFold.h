#pragma once

#include <memory>
#include <utility>
#include <vector>

#include <gsl/gsl_rng.h>

#include "../pgm/DBNData.h"
#include "../utils/Definitions.h"

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
            typedef std::vector<std::pair<DBNData, DBNData>> Split;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a KFold data splitter.
             *
             * @param random_generator
             * @param num_folds
             */
            KFold(std::shared_ptr<gsl_rng> random_generator, int num_folds);

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
             * Creates k data splits comprised of disjoint training and test set.
             *
             * @param data: data to be split
             *
             * @return List of splits
             */
            Split split(const DBNData& data);

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Returns a list of shuffled indices of the data points.
             *
             * @param random_generator: random number generator
             * @param num_data_points: number of data points that will be
             * split into folds
             *
             * @return Shuffled indices of the data points.
             */
            std::vector<int> get_shuffled_indices(int num_data_points) const;

            /**
             * Returns the number of data points in each fold.
             *
             * @param num_data_points: number of data points that will be
             * split into folds
             *
             * @return Number of data points in each one of the folds.
             */
            std::vector<int> get_fold_sizes(int num_data_points) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<gsl_rng> random_generator;

            int num_folds;
        };

    } // namespace model
} // namespace tomcat
