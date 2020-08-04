#include "FileHandler.h"

#include <iostream>

#include <boost/filesystem.hpp>
namespace fs = boost::filesystem;

namespace tomcat {
    namespace model {

#define MAXBUFSIZE static_cast<int>(1e6)
#define DELIMITER ' '

        std::string get_filepath(const std::string& folder_name,
                                 const std::string& filename) {
            fs::path folder(folder_name);
            fs::path file(filename);
            fs::path filepath = folder / file;

            return filepath.string();
        }

        std::string remove_extension(const std::string& filename) {
            return filename.substr(0, filename.find_last_of("."));
        }

        void save_matrix_to_file(const std::string& filepath,
                                 const Eigen::MatrixXd& matrix) {
            std::ofstream file(filepath);
            if (file.is_open()) {
                file << Tensor3::matrix_to_string(matrix);
                file.close();
            }
        }

        Eigen::MatrixXd read_matrix_from_file(const std::string& filepath) {
            //            int cols = 0, rows = 0;
            //            double* buffer = new double[MAXBUFSIZE];
            //
            //            std::ifstream file_reader(filepath);
            //
            //            while (!file_reader.eof()) {
            //                std::string line;
            //                std::getline(file_reader, line);
            //
            //                int temp_cols = 0;
            //                std::stringstream ss(line);
            //                while (!ss.eof()) {
            //                    ss >> buffer[cols * rows + temp_cols++];
            //                }
            //
            //                if (temp_cols == 0) {
            //                    continue;
            //                }
            //
            //                if (cols == 0) {
            //                    cols = temp_cols;
            //                }
            //
            //                rows++;
            //            }
            //
            //            file_reader.close();
            //
            //            Eigen::Map<Eigen::MatrixXd> matrix(buffer, rows,
            //            cols);
            //
            //            return matrix;
            return read_tensor_from_file(filepath)(0);
        }

        void save_tensor_to_file(const std::string& filepath,
                                 const Tensor3& tensor) {
            std::ofstream file(filepath);
            if (file.is_open()) {
                file << tensor;
                file.close();
            }
        }

        Tensor3 read_tensor_from_file(const std::string& filepath) {
            int number_matrices = 0, rows = 0, cols = 0;

            // Final tensor's second and third dimensions
            int d2 = 0, d3 = 0;

            bool started_reading_matrix = false;
            double* buffer = new double[MAXBUFSIZE];
            std::ifstream file_reader(filepath);

            while (true) {
                // if (line.replace(line.begin(), line.end(), " ", "").empty())
                // {
                std::string line;
                if (!file_reader.eof()) {
                    std::getline(file_reader, line);
                }
                std::stringstream ss(line);

                if (file_reader.eof() || ss.eof()) {
                    if (started_reading_matrix) {
                        if (d2 != 0 && d3 != 0) {
                            if (d2 != rows || d3 != cols) {
                                throw std::invalid_argument(
                                    "All the matrixes in a tensor must have "
                                    "the same dimensions.");
                            }
                        }
                        else {
                            // Initialize tensor's second and third dimensions
                            // with the dimensions of the first matrix read.
                            d2 = rows;
                            d3 = cols;
                        }

                        number_matrices++;
                        started_reading_matrix = false;
                        rows = 0;
                        cols = 0;
                    }

                    if (file_reader.eof()) {
                        break;
                    }
                }
                else {
                    int temp_cols = 0;
                    int previous_matrices_size = number_matrices * d2 * d3;

                    while (!ss.eof()) {
                        if (ss.peek() != DELIMITER) {
                            ss >> buffer[rows * cols + previous_matrices_size +
                                         temp_cols++];
                        }
                    }

                    if (temp_cols != 0) {
                        if (cols == 0) {
                            started_reading_matrix = true;
                            cols = temp_cols;
                        }
                        else if (cols != temp_cols) {
                            throw std::invalid_argument(
                                "All the rows in a matrix must have the same "
                                "number of columns.");
                        }
                        rows++;
                    }
                }
            }

            file_reader.close();

            return Tensor3(buffer, number_matrices, d2, d3);
        }

    } // namespace model
} // namespace tomcat
