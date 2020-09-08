#include "Tensor3.h"

#include <iomanip>
#include <sstream>

#include <eigen3/Eigen/Core>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Tensor3::Tensor3() {}

        Tensor3::Tensor3(const Eigen::MatrixXd& matrix) : tensor({matrix}) {}

        /**
         * Creates a tensor comprised of several matrices.
         */
        Tensor3::Tensor3(const vector<Eigen::MatrixXd> matrices)
            : tensor(matrices) {}

        Tensor3::Tensor3(double* buffer, int d1, int d2, int d3) {
            this->tensor.reserve(d1);
            for (int i = 0; i < d1; i++) {
                Eigen::MatrixXd matrix =
                    Eigen::Map<Eigen::Matrix<double,
                                             Eigen::Dynamic,
                                             Eigen::Dynamic,
                                             Eigen::RowMajor>>(buffer, d2, d3);
                this->tensor.push_back(move(matrix));
                if (i < d1 - 1) {
                    buffer += d2 * d3;
                }
            }
        }

        Tensor3::~Tensor3() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        ostream& operator<<(ostream& os, const Tensor3& tensor) {
            for (int m = 0; m < tensor.tensor.size(); m++) {
                const Eigen::MatrixXd& matrix = tensor.tensor[m];

                os << Tensor3::matrix_to_string(matrix);
                os << "\n";
            }

            return os;
        }

        Eigen::MatrixXd Tensor3::operator()(int i, int axis) {
            array<int, 3> shape = this->get_shape();
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
            default: {
                throw TomcatModelException(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return matrix;
        }

        Eigen::VectorXd Tensor3::at(int j, int k) const {
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

        Tensor3 Tensor3::operator+(const Tensor3& tensor) const {
            if (this->get_shape()[0] != tensor.get_shape()[0] ||
                this->get_shape()[1] != tensor.get_shape()[1] ||
                this->get_shape()[2] != tensor.get_shape()[2]) {
                throw TomcatModelException(
                    "It's not possible to sum tensors of different shapes.");
            }

            Tensor3 new_tensor;
            for (int i = 0; i < this->tensor.size(); i++) {
                new_tensor.tensor.push_back(
                    (this->tensor[i].array() + tensor.tensor[i].array())
                        .matrix());
            }

            return new_tensor;
        }

        Tensor3 Tensor3::operator-(const Tensor3& tensor) const {
            if (this->get_shape()[0] != tensor.get_shape()[0] ||
                this->get_shape()[1] != tensor.get_shape()[1] ||
                this->get_shape()[2] != tensor.get_shape()[2]) {
                throw TomcatModelException(
                    "It's not possible to sum tensors of different shapes.");
            }

            Tensor3 new_tensor;
            for (int i = 0; i < this->tensor.size(); i++) {
                new_tensor.tensor.push_back(
                    (this->tensor[i].array() - tensor.tensor[i].array())
                        .matrix());
            }

            return new_tensor;
        }

        Tensor3 Tensor3::operator/(double value) const {
            if (value == 0) {
                throw domain_error(
                    "It's not possible to divide a tensor by 0.");
            }

            Tensor3 new_tensor = *this;
            for (auto& matrix : new_tensor.tensor) {
                matrix = matrix.array() / value;
            }

            return new_tensor;
        }

        Eigen::MatrixXd
        Tensor3::operator==(const Eigen::VectorXd& value) const {
            Tensor3 new_tensor;

            int i = 0;
            for (const auto& matrix : this->tensor) {
                Eigen::MatrixXd new_matrix =
                    (matrix.array() == value(i))
                        .select(
                            Eigen::MatrixXd::Ones(matrix.rows(), matrix.cols()),
                            Eigen::MatrixXd::Zero(matrix.rows(),
                                                  matrix.cols()));
                new_tensor.tensor.push_back(move(new_matrix));
                i++;
            }

            return new_tensor.coeff_wise_and(0)(0, 0);
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

        string Tensor3::matrix_to_string(const Eigen::MatrixXd& matrix) {
            stringstream ss;
            for (int i = 0; i < matrix.rows(); i++) {
                for (int j = 0; j < matrix.cols(); j++) {
                    double value = matrix(i, j);

                    if (int(value) == value) {
                        ss << fixed << setprecision(0) << value;
                    }
                    else {
                        ss << fixed << setprecision(20) << value;
                    }

                    if (j < matrix.cols() - 1) {
                        ss << " ";
                    }
                }
                ss << "\n";
            }

            return ss.str();
        }

        Tensor3 Tensor3::sum(vector<Tensor3> tensors) {
            Tensor3 new_tensor;

            if (!tensors.empty()) {
                new_tensor = tensors[0];

                for (int i = 1; i < tensors.size(); i++) {
                    new_tensor = new_tensor + tensors[i];
                }
            }

            return new_tensor;
        }

        Tensor3 Tensor3::mean(vector<Tensor3> tensors) {
            Tensor3 new_tensor = Tensor3::sum(tensors);
            new_tensor = new_tensor / tensors.size();

            return new_tensor;
        }

        Tensor3 Tensor3::std(vector<Tensor3> tensors) {
            Tensor3 new_tensor;

            if (!tensors.empty()) {
                Tensor3 mean_tensor = Tensor3::mean(tensors);
                new_tensor = (tensors[0] - mean_tensor).pow(2);

                for (int i = 1; i < tensors.size(); i++) {
                    new_tensor = new_tensor + (tensors[i] - mean_tensor).pow(2);
                }

                new_tensor = (new_tensor / tensors.size()).sqrt();
            }

            return new_tensor;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Tensor3::clear() { this->tensor.clear(); }

        bool Tensor3::is_empty() const { return this->tensor.empty(); }

        string Tensor3::to_string() const {
            stringstream ss;
            ss << *this;
            return ss.str();
        }

        int Tensor3::get_size() const {
            return this->get_shape()[0] * this->get_shape()[2] *
                   this->get_shape()[2];
        }

        array<int, 3> Tensor3::get_shape() const {
            int i = this->tensor.size();
            int j = 0;
            int k = 0;
            if (i > 0) {
                j = this->tensor[0].rows();
                k = this->tensor[0].cols();
            }

            return array<int, 3>({i, j, k});
        }

        double Tensor3::at(int i, int j, int k) const {
            return this->tensor[i](j, k);
        }

        Tensor3
        Tensor3::slice(int initial_index, int final_index, int axis) const {
            if (final_index == ALL) {
                final_index = this->get_shape()[axis];
            }

            vector<int> indices;
            indices.reserve(final_index - initial_index);
            for (int idx = initial_index; idx < final_index; idx++) {
                indices.push_back(idx);
            }

            return slice(indices, axis);
        }

        Tensor3 Tensor3::slice(vector<int> indices, int axis) const {
            Tensor3 sliced_tensor;

            switch (axis) {
            case 0: {
                sliced_tensor = *this;
                for (int i = 0; i < indices.size(); i++) {
                    sliced_tensor.tensor.erase(sliced_tensor.tensor.begin() +
                                               indices[i]);
                }

                break;
            }
            case 1: {
                sliced_tensor = *this;
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
                sliced_tensor = *this;
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
            default: {
                throw TomcatModelException(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return sliced_tensor;
        }

        double Tensor3::mean() const {
            double mean = 0;
            for (const auto& matrix : this->tensor) {
                mean += matrix.mean();
            }

            return mean / this->tensor.size();
        }

        Tensor3 Tensor3::mean(int axis) const {
            Tensor3 new_tensor;

            switch (axis) {
            case 0: {

                new_tensor.tensor = vector<Eigen::MatrixXd>();
                for (const auto& matrix : this->tensor) {
                    if (new_tensor.tensor.empty()) {
                        new_tensor.tensor.push_back(matrix);
                    }
                    else {
                        new_tensor.tensor[0] += matrix;
                    }
                }

                new_tensor.tensor[0] =
                    new_tensor.tensor[0].array() / this->tensor.size();
                break;
            }
            case 1: {
                new_tensor = *this;

                for (auto& matrix : new_tensor.tensor) {
                    Eigen::MatrixXd new_matrix(1, matrix.cols());
                    new_matrix.row(0) = matrix.colwise().mean();
                    matrix = new_matrix;
                }
                break;
            }
            case 2: {
                new_tensor = *this;

                for (auto& matrix : new_tensor.tensor) {
                    Eigen::MatrixXd new_matrix(matrix.rows(), 1);
                    new_matrix.col(0) = matrix.rowwise().mean();
                    matrix = new_matrix;
                }
                break;
            }
            default: {
                throw TomcatModelException(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return new_tensor;
        }

        Tensor3 Tensor3::reshape(int d1, int d2, int d3) const {
            if (d1 * d2 * d3 != this->get_size()) {
                throw TomcatModelException(
                    "New dimensions must result in a tensor of the same size.");
            }

            double* buffer = new double[d1 * d2 * d3];
            for (auto& matrix : this->tensor) {
                for (int i = 0; i < matrix.rows(); i++) {
                    for (int j = 0; j < matrix.cols(); j++) {
                        *buffer = matrix(i, j);
                        buffer++;
                    }
                }
            }

            buffer -= d1 * d2 * d3;
            return Tensor3(buffer, d1, d2, d3);
        }

        Tensor3 Tensor3::repeat(int num_repetitions, int axis) const {
            Tensor3 new_tensor;

            switch (axis) {
            case 0: {
                new_tensor = *this;

                for (int r = 0; r < num_repetitions - 1; r++) {
                    for (const auto& matrix : this->tensor) {
                        new_tensor.tensor.push_back(matrix);
                    }
                }
                break;
            }
            case 1: {
                new_tensor = *this;

                for (auto& matrix : new_tensor.tensor) {
                    Eigen::MatrixXd new_matrix(num_repetitions, matrix.cols());
                    new_matrix = matrix.colwise().replicate(num_repetitions);
                    matrix = new_matrix;
                }
                break;
            }
            case 2: {
                new_tensor = *this;

                for (auto& matrix : new_tensor.tensor) {
                    Eigen::MatrixXd new_matrix(matrix.rows(), num_repetitions);
                    new_matrix = matrix.rowwise().replicate(num_repetitions);
                    matrix = new_matrix;
                }
                break;
            }
            default: {
                throw TomcatModelException(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return new_tensor;
        }

        Tensor3 Tensor3::pow(int power) const {
            Tensor3 new_tensor = *this;

            for (auto& matrix : new_tensor.tensor) {
                matrix = matrix.array().pow(power).matrix();
            }

            return new_tensor;
        }

        Tensor3 Tensor3::sqrt() const {
            Tensor3 new_tensor = *this;

            for (auto& matrix : new_tensor.tensor) {
                matrix = matrix.array().sqrt().matrix();
            }

            return new_tensor;
        }

        Tensor3 Tensor3::coeff_wise_and(int axis) const {
            auto [d1, d2, d3] = this->get_shape();
            Tensor3 new_tensor;

            switch (axis) {
            case 0: {
                new_tensor = Tensor3::constant(1, d2, d3, 1);
                for (int j = 0; j < d2; j++) {
                    for (int k = 0; k < d3; k++) {
                        for (int i = 0; i < d1; i++) {
                            if (this->tensor[i](j, k) <= 0) {
                                new_tensor.tensor[0](j, k) = 0;
                                break;
                            }
                        }
                    }
                }
                break;
            }
            case 1: {
                new_tensor = Tensor3::constant(d1, 1, d3, 1);
                for (int i = 0; i < d1; i++) {
                    for (int k = 0; k < d3; k++) {
                        for (int j = 0; j < d2; j++) {
                            if (this->tensor[i](j, k) <= 0) {
                                new_tensor.tensor[i](0, k) = 0;
                                break;
                            }
                        }
                    }
                }
                break;
            }
            case 2: {
                new_tensor = Tensor3::constant(d1, d2, 1, 1);
                for (int i = 0; i < d1; i++) {
                    for (int j = 0; j < d2; j++) {
                        for (int k = 0; k < d3; k++) {
                            if (this->tensor[i](j, k) <= 0) {
                                new_tensor.tensor[i](j, 0) = 0;
                                break;
                            }
                        }
                    }
                }
                break;
            }
            default: {
                throw TomcatModelException(
                    "Invalid axis. Valid axes are 0, 1 or 2.");
            }
            }

            return new_tensor;
        }

    } // namespace model
} // namespace tomcat
