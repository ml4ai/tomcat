#include "FileHandler.h"

#include <iostream>

#include <boost/filesystem.hpp>
namespace fs=boost::filesystem;

namespace tomcat {
    namespace model {

#define MAXBUFSIZE static_cast<int>(1e6)

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
                file << matrix;
                file.close();
            }
        }

        Eigen::MatrixXd read_matrix_from_file(const std::string& filepath) {
            int cols = 0, rows = 0;
            double* buffer = new double[MAXBUFSIZE];

            std::ifstream file_reader(filepath);

            while (!file_reader.eof()) {
                std::string line;
                std::getline(file_reader, line);

                int temp_cols = 0;
                std::stringstream ss(line);
                while (!ss.eof()) {
                    ss >> buffer[cols * rows + temp_cols++];
                }

                if (temp_cols == 0) {
                    continue;
                }

                if (cols == 0) {
                    cols = temp_cols;
                }

                rows++;
            }

            file_reader.close();

            Eigen::Map<Eigen::MatrixXd> matrix(buffer, rows, cols);

            return matrix;
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
            //            cols); delete[] buffer;
            //
            //            return matrix;
            return Tensor3();
        }

    } // namespace model
} // namespace tomcat
