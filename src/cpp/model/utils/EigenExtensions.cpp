#include "EigenExtensions.h"

#include <sstream>

namespace tomcat {
    namespace model {

        using namespace std;

        Eigen::MatrixXd cum_sum(const Eigen::MatrixXd& matrix, int axis) {
            Eigen::MatrixXd new_matrix(matrix.rows(), matrix.cols());

            switch (axis) {
            case 0: {
                Eigen::VectorXd cum_sum = Eigen::VectorXd::Zero(matrix.cols());
                for (int i = 0; i < matrix.rows(); i++) {
                    new_matrix.row(i) = matrix.row(i) + cum_sum;
                    cum_sum = new_matrix.row(i);
                }
                break;
            }
            case 1: {
                Eigen::VectorXd cum_sum = Eigen::VectorXd::Zero(matrix.rows());
                for (int j = 0; j < matrix.cols(); j++) {
                    new_matrix.col(j) = matrix.col(j) + cum_sum;
                    cum_sum = new_matrix.col(j);
                }
                break;
            }
            default: {
                throw invalid_argument(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return new_matrix;
        }

        Eigen::MatrixXd mean(const vector<Eigen::MatrixXd>& matrices) {
            Eigen::MatrixXd mean_matrix;

            if (!matrices.empty()) {
                mean_matrix = matrices[0];

                for (int i = 1; i < matrices.size(); i++) {
                    mean_matrix =
                        (mean_matrix.array() + matrices[i].array()).matrix();
                }

                mean_matrix = (mean_matrix.array() / matrices.size()).matrix();
            }

            return mean_matrix;
        }

        Eigen::MatrixXd
        standard_error(const vector<Eigen::MatrixXd>& matrices){
            Eigen::MatrixXd mean_matrix = mean(matrices);
            Eigen::MatrixXd se_matrix = (matrices[0].array() - mean_matrix.array()).pow(2).matrix();

            for (int i = 1; i < matrices.size(); i++) {
                se_matrix = se_matrix + (matrices[i].array() - mean_matrix.array()).pow(2).matrix();
            }

            se_matrix = (se_matrix.array().sqrt() / matrices.size()).matrix();

            return se_matrix;
        }

        string to_string(const Eigen::VectorXd& vector) {
            stringstream ss;
            ss << vector;
            return ss.str();
        }

        string to_string(const Eigen::MatrixXd& matrix) {
            stringstream ss;
            ss << matrix;
            return ss.str();
        }

    } // namespace model
} // namespace tomcat
