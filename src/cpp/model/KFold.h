#pragma once

#include <utility>
#include <vector>

#include <gsl/gsl_rng.h>

#include "EvidenceSet.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Class description here
         */
        class KFold {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------
            KFold(int num_folds);

            ~KFold();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            KFold(const KFold&) = default;

            KFold& operator=(const KFold&) = default;

            KFold(KFold&&) = default;

            KFold& operator=(KFold&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::vector<std::pair<EvidenceSet, EvidenceSet>>
            split(std::shared_ptr<gsl_rng> random_generator,
                  const EvidenceSet& data);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

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
            std::vector<int> get_shuffled_indices(std::shared_ptr<gsl_rng> random_generator,
                                      int num_data_points) const;

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
            int num_folds;
        };

    } // namespace model
} // namespace tomcat
