#include "Tensor3.h"

#include <iomanip>

#include <eigen3/Eigen/Core>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Tensor3::Tensor3() {}

        Tensor3::Tensor3(double* buffer, int d1, int d2, int d3) {
            this->tensor.reserve(d1);
            for (int i = 0; i < d1; i++) {
                Eigen::MatrixXd matrix =
                    Eigen::Map<Eigen::Matrix<double,
                                             Eigen::Dynamic,
                                             Eigen::Dynamic,
                                             Eigen::RowMajor>>(buffer, d2, d3);
                this->tensor.push_back(std::move(matrix));
                if (i < d1 - 1) {
                    buffer += d2 * d3;
                }
            }
        }

        Tensor3::~Tensor3() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        std::ostream& operator<<(std::ostream& os, const Tensor3& tensor) {
            for (int m = 0; m < tensor.tensor.size(); m++) {
                const Eigen::MatrixXd& matrix = tensor.tensor[m];

                os << Tensor3::matrix_to_string(matrix);
                os << "\n";
            }

            return os;
        }

        Eigen::MatrixXd Tensor3::operator()(int i, int axis) {
            std::array<int, 3> shape = this->get_shape();
            Eigen::MatrixXd matrix;

            switch (axis) {
            case 0: {
                matrix = this->tensor[i];
                break;
            }
            case 1: {
                int j = i;
                matrix = Eigen::MatrixXd(shape[0], shape[2]);
                for (int i = 0; i < this->tensor.size(); i++) {
                    matrix.row(i) = this->tensor[i].row(j);
                }
                break;
            }
            case 2: {
                int k = i;
                matrix = Eigen::MatrixXd(shape[0], shape[1]);
                for (int i = 0; i < this->tensor.size(); i++) {
                    matrix.row(i) = this->tensor[i].col(k).transpose();
                }
                break;
            }
            default:
                throw std::invalid_argument(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }

            return matrix;
        }

        Eigen::VectorXd Tensor3::operator()(int j, int k) const {
            size_t vector_size = this->tensor.size();
            double* buffer = new double[vector_size];

            for (int i = 0; i < vector_size; i++) {
                buffer[i] = this->tensor[i](j, k); //(*this)(i, j, k);
            }

            Eigen::Map<Eigen::VectorXd> vector(buffer, vector_size);
            delete[] buffer;

            return vector;
        }

        double& Tensor3::operator()(int i, int j, int k) {
            return this->tensor[i](j, k);
        }

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        Tensor3 Tensor3::constant(int d1, int d2, int d3, double value) {
            double* buffer = new double[d1 * d2 * d3];

            for (int i = 0; i < d1 * d2 * d3; i++) {
                buffer[i] = value;
            }

            Tensor3 tensor(buffer, d1, d2, d3);

            return tensor;
        }

        std::string Tensor3::matrix_to_string(const Eigen::MatrixXd& matrix) {
            std::stringstream ss;
            for (int i = 0; i < matrix.rows(); i++) {
                for (int j = 0; j < matrix.cols(); j++) {
                    double value = matrix(i, j);

                    if (int(value) == value) {
                        ss << std::fixed << std::setprecision(0) << value;
                    }
                    else {
                        ss << std::fixed << std::setprecision(20) << value;
                    }

                    if (j < matrix.cols() - 1) {
                        ss << " ";
                    }
                }
                ss << "\n";
            }

            return ss.str();
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::array<int, 3> Tensor3::get_shape() const {
            int i = this->tensor.size();
            int j = 0;
            int k = 0;
            if (i > 0) {
                j = this->tensor[0].rows();
                k = this->tensor[0].cols();
            }

            return std::array<int, 3>({i, j, k});
        }

        Tensor3 Tensor3::slice(std::vector<int> indices, int axis) const {
            Tensor3 sliced_tensor = *this;

            switch (axis) {
            case 0: {
                for (int i = 0; i < indices.size(); i++) {
                    sliced_tensor.tensor.erase(sliced_tensor.tensor.begin() +
                                               indices[i]);
                }

                break;
            }
            case 1: {
                for (auto& matrix : sliced_tensor.tensor) {
                    Eigen::MatrixXd sliced_matrix(indices.size(),
                                                  matrix.cols());

                    for (int j = 0; j < indices.size(); j++) {
                        sliced_matrix.row(j) = matrix.row(indices[j]);
                    }

                    matrix = sliced_matrix;
                }
                break;
            }
            case 2: {
                for (auto& matrix : sliced_tensor.tensor) {
                    Eigen::MatrixXd sliced_matrix(matrix.rows(),
                                                  indices.size());

                    for (int k = 0; k < indices.size(); k++) {
                        sliced_matrix.col(k) = matrix.col(indices[k]);
                    }

                    matrix = sliced_matrix;
                }
                break;
            }
            }

            return sliced_tensor;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        // No definitions in this file

    } // namespace model
} // namespace tomcat