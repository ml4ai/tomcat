#include "Tensor3.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Tensor3::Tensor3(double* buffer, int d1, int d2, int d3) {
            this->tensor.reserve(d1);
            for (int i = 0; i < d1; i++) {
                Eigen::MatrixXd matrix =
                    Eigen::Map<Eigen::MatrixXd>(buffer, d2, d3);
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
            os << "[";
            for (int m = 0; m < tensor.tensor.size(); m++) {
                const Eigen::MatrixXd& matrix = tensor.tensor[m];

                os << "[";
                for (int i = 0; i < matrix.rows(); i++){
                    os << matrix.row(i);
                    if (i == matrix.rows() - 1){
                        os << "]";
                    } else{
                        os << "\n";
                        os << "  ";
                    }
                }
                if (m == tensor.tensor.size() - 1){
                    os << "]";
                } else{
                    os << "\n";
                    os << "  ";
                }
            }

            return os;
        }

        Eigen::MatrixXd& Tensor3::operator()(int i) { return this->tensor[i]; }

        Eigen::VectorXd Tensor3::operator()(int j, int k) {
            size_t vector_size = this->tensor.size();
            double* buffer = new double[vector_size];

            for (int i = 0; i < vector_size; i++) {
                buffer[i] = (*this)(i, j, k);
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

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        // No definitions in this file

    } // namespace model
} // namespace tomcat
