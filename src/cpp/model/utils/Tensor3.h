#pragma once

#include <array>
#include <iostream>
#include <vector>

#include <eigen3/Eigen/Dense>

#include "model/utils/Definitions.h"

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
            static const int ALL = -1;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty tensor.
             */
            Tensor3();

            /**
             * Creates tensor with one matrix.
             */
            Tensor3(const Eigen::MatrixXd& matrix);

            /**
             * Creates a tensor comprised of several matrices.
             */
            Tensor3(const std::vector<Eigen::MatrixXd> matrices);

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
             * @param i: index to select in the chosen axis
             * @param axis: axis
             *
             * @return Assignable matrix.
             */
            Eigen::MatrixXd operator()(int i, int axis);

            /**
             * Returns a non-assignable vector for given indices of the second
             * and third axes.
             *
             * @param j: second axis' index
             * @param k: third axis' index
             * @return Non-assignable vector.
             */
            Eigen::VectorXd at(int j, int k) const;

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

            /**
             * Returns the tensor formed by the element-wise sum of two tensors.
             *
             * @param tensor: another tensor
             *
             * @return Element-wise sum of two tensors.
             */
            Tensor3 operator+(const Tensor3& tensor) const;

            /**
             * Returns the tensor formed by the element-wise subtraction of two
             * tensors.
             *
             * @param tensor: another tensor
             *
             * @return Element-wise subtraction of two tensors.
             */
            Tensor3 operator-(const Tensor3& tensor) const;

            /**
             * Returns the tensor formed by the element-wise scalar division of
             * a tensor.
             *
             * @param value: value to divide the elements of a tensor
             *
             * @return Element-wise scalar division of a tensor.
             */
            Tensor3 operator/(double value) const;

            /**
             * Returns a 1 x m x n tensor comprised by ones where the
             * coefficients along axis 0 are equal to value, and zero otherwise.
             *
             * @param value: value to compare the coefficients of axis 0 with
             *
             * @return Binary tensor
             */
            Eigen::MatrixXd operator==(const Eigen::VectorXd& value) const;

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

            /**
             * Computes the element-wise sum of a list of tensors.
             *
             * @param tensors: tensors
             *
             * @return: Element-wise sum of a list of tensors.
             */
            static Tensor3 sum(std::vector<Tensor3> tensors);

            /**
             * Computes the element-wise mean of a list of tensors.
             *
             * @param tensors: tensors
             *
             * @return: Element-wise mean of a list of tensors.
             */
            static Tensor3 mean(std::vector<Tensor3> tensors);

            /**
             * Computes the element-wise standard deviation of a list of
             * tensors.
             *
             * @param tensors: tensors
             *
             * @return: Element-wise standard deviation of a list of tensors.
             */
            static Tensor3 std(std::vector<Tensor3> tensors);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Clears the tensor;
             */
            void clear();

            /**
             * Checks whether a tensor is empty.
             */
            bool is_empty() const;

            /**
             * Returns the content of the tensor as a string;
             *
             * @return String with the tensor's content.
             */
            std::string to_string() const;

            /**
             * Returns the number of elements in the tensor.
             *
             * @return The number of elements in the tensor.
             */
            int get_size() const;

            /**
             * Returns 3D array containing the dimensions of the tensor.
             *
             * @return Tensor's dimensions.
             */
            std::array<int, 3> get_shape() const;

            /**
             * Returns a non-assignable coefficient from a specific tensor
             * index.
             *
             * @param i: first axis' index
             * @param j: second axis' index
             * @param k: third axis' index
             *
             * @return Non-assignable tensor coefficient.
             */
            double at(int i, int j, int k) const;

            /**
             * Returns a sliced copy of the tensor in a certain range.
             *
             * @param initial_index: first index in the range (inclusive). The
             * element in this index is kept.
             * @param final_index: last index in the range (exclusive). The
             * element in this index is not kept.
             * @param axis: axis where the slicing must be done
             *
             * @return Sliced copy of the original tensor.
             */
            Tensor3 slice(int initial_index, int final_index, int axis) const;

            /**
             * Returns a sliced copy of the tensor.
             *
             * @param indices: indices to keep from the original tensor
             * @param axis: axis where the slicing must be done
             *
             * @return Sliced copy of the original tensor.
             */
            Tensor3 slice(std::vector<int> indices, int axis) const;

            /**
             * Returns the mean of all the values in the tensor.
             *
             * @return Mean.
             */
            double mean() const;

            /**
             * Returns the mean of all the values in the tensor in a given
             * axis. This operation shrinks the number of elements in the axis
             * where the operation is performed but it preserves the number of
             * axes.
             *
             * @param axis: axis where the mean must be computed along
             *
             * @return Mean.
             */
            Tensor3 mean(int axis) const;

            /**
             * Reshapes the tensor if new dimensions are compatible with the
             * number of elements in the tensor.
             *
             * @param d1: dimension of the first axis
             * @param d2: dimension of the second axis
             * @param d3: dimension of the third axis
             *
             * @return Reshaped tensor.
             */
            Tensor3 reshape(int d1, int d2, int d3) const;

            /**
             * Repeats the elements of the tensor in a given axis by a certain
             * amount of time.
             *
             * @param num_repetitions: number of repetitions
             * @param axis: axis where the repetitions must occur
             *
             * @return Repeated tensor.
             */
            Tensor3 repeat(int num_repetitions, int axis) const;

            /**
             * Returns a tensor formed by the elements of the original tensor to
             * a given power.
             *
             * @param power: power
             *
             * @return Tensor formed by the elements of the original tensor to a
             * given power.
             */
            Tensor3 pow(int power) const;

            /**
             * Returns a tensor formed by the squared root of the elements of
             * the original tensor.
             *
             * @return Tensor formed by the squared root of the elements of the
             * original tensor.
             */
            Tensor3 sqrt() const;

            /**
             * Returns the bitwise-and of all the coefficients in a tensor in a
             * given axis. All numbers greater greater 0 are considered as true
             * for the purpose of this logical operation. Negative numbers are
             * neutral and preserved in the final tensor. This operation shrinks
             * the number of coefficients in the axis where the operation is
             * performed but it preserves the number of axes.
             *
             * @param axis: axis where the bitwise-and must be computed along
             *
             * @return Mean.
             */
            Tensor3 coeff_wise_and(int axis) const;

          private:
            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<Eigen::MatrixXd> tensor;
        };

    } // namespace model
} // namespace tomcat
