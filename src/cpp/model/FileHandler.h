#pragma once

#import <fstream>
#import <string>

#import <eigen3/Eigen/Dense>

namespace tomcat {
    namespace model {

        /**
         * Returns the absolute path of a file given its name and folder where
         * it's stored.
         *
         * @param folder_name: name of the folder the file is stored in
         * @param filename: name of the file
         * @return File's absolute path.
         */
        std::string get_filepath(const std::string& folder_name,
                                 const std::string& filename);

        /**
         * Removes the last extension from a file.
         *
         * @param filename: name of the file
         * @return Filename without extension.
         */
        std::string remove_extension(const std::string& filename);

        /**
         * Saves a matrix of numeric values to a file.
         *
         * @param filepath: path of the file where the matrix must be written to
         * @param matrix: matrix of numeric values
         */
        void save_matrix_to_file(const std::string& filepath,
                                 const Eigen::MatrixXd& matrix);

        /**
         * Reads a matrix of numeric values from a file.
         *
         * @param filepath: path of the file where the matrix is stored
         * @return Matrix of numeric values.
         */
        Eigen::MatrixXd read_matrix_from_file(const std::string& filepath);

    } // namespace model
} // namespace tomcat