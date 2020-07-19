#pragma once

#include "CategoricalCPD.h"
#include "Node.h"
#include "NodeMetadata.h"
#include <memory>

namespace tomcat {
    namespace model {

        /*
         * A numeric random variable node is a concrete node in the unrolled DBN
         * that has a distribution from which it can be sampled from, yielding a
         * 1D number.
         */

//        class RandomVariableNumericNode : public Node<double> {
//          private:
//            std::shared_ptr<NodeMetadata> metadata;
//            std::unique_ptr<CPD<Eigen::MatrixXd>> cpd;
//            int time_step;
//            double assignment;
//
//          public:
//            RandomVariableNumericNode(std::shared_ptr<NodeMetadata> metadata,
//                                      std::unique_ptr<CPD<Eigen::MatrixXd>> cpd,
//                                      int time_step = 0)
//                : metadata(std::move(metadata)), cpd(std::move(cpd)),
//                  time_step(time_step) {}
//
//            ~RandomVariableNumericNode() {}
//
//            double get_assignment() const override;
//
//            void print(std::ostream& os) const override;
//
//            // Getters
//            const std::shared_ptr<NodeMetadata>& get_metadata() const {
//                return metadata;
//            }
//
//            const std::unique_ptr<CPD<Eigen::MatrixXd>>& get_cpd() const {
//                return cpd;
//            }
//
//            int get_time_step() const { return time_step; }
//
//            // Setters
//            void set_time_step(int time_step) {
//                RandomVariableNumericNode::time_step = time_step;
//            }
//
//            void set_assignment(double assignment) {
//                RandomVariableNumericNode::assignment = assignment;
//            }
//        };

    } // namespace model
} // namespace tomcat