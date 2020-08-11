#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

//        //----------------------------------------------------------------------
//        // Definitions
//        //----------------------------------------------------------------------
//
//        // No definitions in this file
//
//        //----------------------------------------------------------------------
//        // Constructors & Destructor
//        //----------------------------------------------------------------------
//        ContinuousCPD::ContinuousCPD() {}
//
//        ContinuousCPD::ContinuousCPD(
//            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order)
//            : CPD(parent_node_order) {}
//
//        ContinuousCPD::ContinuousCPD(
//            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order)
//            : CPD(std::move(parent_node_order)) {}
//
//        ContinuousCPD::~ContinuousCPD() {}
//
//        //----------------------------------------------------------------------
//        // Member functions
//        //----------------------------------------------------------------------
//        void
//        ContinuousCPD::update_dependencies(Node::NodeMap& parameter_nodes_map,
//                                           int time_step) {
//            for (int i = 0; i < this->distributions.size(); i++) {
//                for (int j = 0;
//                     j < this->distributions[i]->get_parameters().size();
//                     j++) {
//                    std::string parameter_timed_name;
//                    NodeMetadata* metadata = this->distributions[i]
//                                                 ->get_parameters()[j]
//                                                 ->get_metadata()
//                                                 .get();
//                    if (metadata->is_replicable()) {
//                        parameter_timed_name =
//                            metadata->get_timed_name(time_step);
//                    }
//                    else {
//                        parameter_timed_name = metadata->get_timed_name(
//                            metadata->get_initial_time_step());
//                    }
//
//                    if (parameter_nodes_map.count(parameter_timed_name) > 0) {
//                        this->distributions[i]->set_parameter(
//                            parameter_nodes_map[parameter_timed_name], j);
//                    }
//                    delete metadata;
//                }
//            }
//            this->updated = true;
//        }
//
//        void ContinuousCPD::copy_from_cpd(const ContinuousCPD& cpd) {
//            CPD::copy_from_cpd(cpd);
//            this->parent_node_order = cpd.parent_node_order;
//            this->distributions = cpd.distributions;
//        }
//
//        // TODO - Transfer implementation to derived classes
////        void ContinuousCPD::clone_distributions() {
////            for (auto& distribution : this->parameter_table) {
////                std::shared_ptr<Distribution> temp = distribution->clone();
////                distribution = std::dynamic_pointer_cast<ContinuousCPD>(temp);
////            }
////        }

    } // namespace model
} // namespace tomcat
