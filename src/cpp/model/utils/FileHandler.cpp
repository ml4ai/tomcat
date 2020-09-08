#include "FileHandler.h"

#include <iostream>
#include <sstream>

#include <boost/filesystem.hpp>
namespace fs = boost::filesystem;

namespace tomcat {
    namespace model {

        using namespace std;

#define MAXBUFSIZE static_cast<int>(1e6)
#define DELIMITER ' '

        string get_filepath(const string& folder_name,
                                 const string& filename) {
            fs::path folder(folder_name);
            fs::path file(filename);
            fs::path filepath = folder / file;

            return filepath.string();
        }

        string remove_extension(const string& filename) {
            return filename.substr(0, filename.find_last_of("."));
        }

        void save_matrix_to_file(const string& filepath,
                                 const Eigen::MatrixXd& matrix) {
            ofstream file(filepath);
            if (file.is_open()) {
                file << Tensor3::matrix_to_string(matrix);
                file.close();
            }
        }

        Eigen::MatrixXd read_matrix_from_file(const string& filepath) {
            return read_tensor_from_file(filepath)(0, 0);
        }

        void save_tensor_to_file(const string& filepath,
                                 const Tensor3& tensor) {
            ofstream file(filepath);
            if (file.is_open()) {
                file << tensor;
                file.close();
            }
        }

        Tensor3 read_tensor_from_file(const string& filepath) {
            int number_matrices = 0, rows = 0, cols = 0;

            // Final tensor's second and third dimensions
            int d2 = 0, d3 = 0;

            bool started_reading_matrix = false;
            double* buffer = new double[MAXBUFSIZE];
            ifstream file_reader(filepath);

            while (true) {
                // if (line.replace(line.begin(), line.end(), " ", "").empty())
                // {
                string line;
                if (!file_reader.eof()) {
                    getline(file_reader, line);
                }
                stringstream ss(line);

                if (file_reader.eof() || ss.eof()) {
                    if (started_reading_matrix) {
                        if (d2 != 0 && d3 != 0) {
                            if (d2 != rows || d3 != cols) {
                                throw invalid_argument(
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
                    double value;

                    while (!ss.eof()) {
                        ss >> value;
                        if (!ss.fail()) {
                            buffer[rows * cols + previous_matrices_size +
                                         temp_cols++] = value;
                        }
                    }

                    if (temp_cols != 0) {
                        if (cols == 0) {
                            started_reading_matrix = true;
                            cols = temp_cols;
                        }
                        else if (cols != temp_cols) {
                            throw invalid_argument(
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
