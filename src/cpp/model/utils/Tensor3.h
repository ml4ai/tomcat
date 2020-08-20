#pragma once

#include <array>
#include <vector>

#include <eigen3/Eigen/Dense>

#include <iostream>

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
        class Tensor3 {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty tensor.
             */
            Tensor3();

            /**
             * Creates a tensor filled with data.
             *
             * @param buffer: values to be stored in the tensor. There must be
             * d1*d2*d3 elements in the buffer array.
             * @param d1: dimension of the first axis
             * @param d2: dimension of the second axis
             * @param d3: dimension of the third axis
             */
            Tensor3(double* buffer, int d1, int d2, int d3);

            ~Tensor3();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Tensor3(const Tensor3&) = default;

            Tensor3& operator=(const Tensor3&) = default;

            Tensor3(Tensor3&&) = default;

            Tensor3& operator=(Tensor3&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------
            friend std::ostream& operator<<(std::ostream& os,
                                            const Tensor3& tensor);

            /**
             * Returns assignable matrix for a given index of the first axis.
             *
             * @param i: first axis' index
             *
             * @return Assignable matrix.
             */
            Eigen::MatrixXd operator()(int i, int axis = 0);

            /**
             * Returns a non-assignable vector for given indices of the second
             * and third axes.
             *
             * @param j: second axis' index
             * @param k: third axis' index
             * @return Non-assignable vector.
             */
            Eigen::VectorXd operator()(int j, int k) const;

            /**
             * Returns an assignable number given indices of all axes.
             *
             * @param i: first axis' index
             * @param j: second axis' index
             * @param k: third axis' index
             *
             * @return Assignable number.
             */
            double& operator()(int i, int j, int k);

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------
            /**
             * Creates a tensor filled with a constant value.
             *
             * @param d1: dimension of the first axis
             * @param d2: dimension of the second axis
             * @param d3: dimension of the third axis
             * @param value: constant value
             * @return Tensor of constant values.
             */
            static Tensor3 constant(int d1, int d2, int d3, double value);

            /**
             * Returns a string representation for a matrix;
             *
             * @param matrix: matrix
             * @return Matrix's string representation.
             */
            static std::string matrix_to_string(const Eigen::MatrixXd& matrix);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Returns 3D array containing the dimensions of the tensor.
             *
             * @return Tensor's dimensions.
             */
            std::array<int, 3> get_shape() const;

            /**
             * Returns a sliced copy of the tensor.
             *
             * @param indices: indices to keep from the original tensor
             * @param axis: axis where the slicing must be done
             *
             * @return Sliced copy of the original tensor.
             */
            Tensor3 slice(std::vector<int> indices, int axis = 0) const;

          private:
            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<Eigen::MatrixXd> tensor;
        };

    } // namespace model
} // namespace tomcat
