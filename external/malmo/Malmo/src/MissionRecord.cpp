// --------------------------------------------------------------------------------------------------
//  Copyright (c) 2016 Microsoft Corporation
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
// --------------------------------------------------------------------------------------------------

// Local:
#include "MissionRecord.h"
#include "Logger.h"

// Boost:
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

// STL:
#include <exception>
#include <iostream>

#define LOG_COMPONENT Logger::LOG_RECORDING

using namespace std;
using namespace boost::filesystem;

namespace malmo {
    MissionRecord::MissionRecord(const MissionRecordSpec& spec)
        : spec(spec), is_closed(true) {
        if (spec.isRecording()) {
            boost::uuids::random_generator gen;
            boost::uuids::uuid temp_uuid = gen();
            char* malmo_tmp_path = getenv("MALMO_TEMP_PATH");
            if (malmo_tmp_path)
                this->temp_dir = path(malmo_tmp_path);
            else
                this->temp_dir = path(".");
            this->mission_id = boost::uuids::to_string(temp_uuid);
            this->temp_dir =
                this->temp_dir / "mission_records" / this->mission_id;
            this->mp4_path = (this->temp_dir / "video.mp4").string();
            this->mp4_depth_path =
                (this->temp_dir / "depth_video.mp4").string();
            this->mp4_luminance_path =
                (this->temp_dir / "luminance_video.mp4").string();
            this->mp4_colourmap_path =
                (this->temp_dir / "colourmap_video.mp4").string();
            this->observations_path =
                (this->temp_dir / "observations.txt").string();
            this->rewards_path = (this->temp_dir / "rewards.txt").string();
            this->commands_path = (this->temp_dir / "commands.txt").string();
            this->mission_init_path =
                (this->temp_dir / "missionInit.xml").string();
            this->mission_ended_path =
                (this->temp_dir / "missionEnded.xml").string();
            bool created_tmp = false;
            try {
                created_tmp = create_directories(this->temp_dir);
            }
            catch (const exception& e) {
                LOGERROR(LT("Unable to create temporary folder for recording "),
                         this->temp_dir.string(),
                         LT(": "),
                         e.what());
                cout << "Unable to create temporary folder for recording "
                     << this->temp_dir.string() << ": " << e.what() << endl;
                throw runtime_error(
                    "Check your MALMO_TEMP_PATH and try again.");
            }

            if (created_tmp) {
                this->is_closed = false;
            }
            else {
                LOGERROR(LT("Unable to create temporary folder for recording "),
                         this->temp_dir.string());
                throw runtime_error(
                    "Unable to create temporary folder for recording " +
                    this->temp_dir.string() + ": check your MALMO_TEMP_PATH?");
            }
        }
    }

    MissionRecord::~MissionRecord() {
        try {
            if (!this->is_closed) {
                this->close();
            }
        }
        catch (const exception& e) {
            // we don't really want to assume that is safe to write to cout but
            // can't have destructors throwing exceptions
            cout << "Exception in closing of MissionRecord: " << e.what()
                 << endl;
            LOGERROR(LT("Exception in closing of MissionRecord: "), e.what());
        }
        catch (...) {
            // we don't really want to assume that is safe to write to cout but
            // can't have destructors throwing exceptions
            cout << "Unknown exception in closing of MissionRecord." << endl;
            LOGERROR(LT("Unknown exception in closing of MissionRecord."));
        }
    }

    MissionRecord::MissionRecord(MissionRecord&& record)
        : is_closed(record.is_closed), spec(record.spec),
          commands_path(record.commands_path), mp4_path(record.mp4_path),
          mp4_depth_path(record.mp4_depth_path),
          mp4_luminance_path(record.mp4_luminance_path),
          mp4_colourmap_path(record.mp4_colourmap_path),
          observations_path(record.observations_path),
          rewards_path(record.rewards_path),
          mission_init_path(record.mission_init_path),
          mission_ended_path(record.mission_ended_path),
          temp_dir(record.temp_dir), mission_id(record.mission_id) {
        record.spec = MissionRecordSpec();
    }

    MissionRecord& MissionRecord::operator=(MissionRecord&& record) {
        if (this != &record) {
            this->is_closed = record.is_closed;
            this->spec = record.spec;
            this->commands_path = record.commands_path;
            this->mp4_path = record.mp4_path;
            this->mp4_depth_path = record.mp4_depth_path;
            this->mp4_luminance_path = record.mp4_luminance_path;
            this->mp4_colourmap_path = record.mp4_colourmap_path;
            this->observations_path = record.observations_path;
            this->rewards_path = record.rewards_path;
            this->mission_init_path = record.mission_init_path;
            this->mission_ended_path = record.mission_ended_path;
            this->temp_dir = record.temp_dir;
            this->mission_id = record.mission_id;

            record.spec = MissionRecordSpec();
        }

        return *this;
    }

    void MissionRecord::close() {
        if (!this->spec.isRecording() || this->is_closed) {
            return;
        }

        LOGSECTION(LOG_INFO, LT("Closing MissionRecord..."));
        // create zip file, push to destination
        vector<path> fileList;
        this->addFiles(fileList, this->temp_dir);
        LOGINFO(LT("Found "), fileList.size(), LT(" files to tar:"));

        if (fileList.size() > 0) {
            std::ofstream file(this->spec.destination, std::ofstream::binary);
            if (file.fail()) {
                cout << "[warning] Unable to write recording to output file "
                     << this->spec.destination << endl;
                LOGERROR(LT("Unable to write recording to output file "),
                         this->spec.destination);
            }
            else {
                boost::iostreams::filtering_ostream out;
                out.push(boost::iostreams::gzip_compressor());
                out.push(file);
                lindenb::io::Tar tarball(out);

                for (auto file : fileList) {
                    try {
                        this->addFile(tarball, file);
                    }
                    catch (const exception& e) {
                        cout << "[warning] Unable to archive " << file.string()
                             << ": " << e.what() << endl;
                        LOGERROR(LT("Unable to archive "),
                                 file.string(),
                                 LT(": "),
                                 e.what());
                    }
                }

                tarball.finish();
                if (!out.good()) {
                    cout << "[warning] tar file corrupted." << endl;
                    LOGERROR(LT("Tar file corrupted."));
                }
            }
        }
        LOGINFO(LT("Deleting MissionRecord temp folder."));
        remove_all(this->temp_dir);

        this->is_closed = true;
    }

    void MissionRecord::addFiles(vector<path>& fileList, path directory) {
        if (!exists(directory)) {
            LOGERROR(LT("Attempt to write to non-existent directory: "),
                     directory.string());
            throw runtime_error("Attempt to write to non-existent directory: " +
                                directory.string());
        }
        directory_iterator dirIter(directory);
        directory_iterator dirIterEnd;

        while (dirIter != dirIterEnd) {
            if (exists(*dirIter)) {
                if (is_directory(*dirIter)) {
                    this->addFiles(fileList, *dirIter);
                }
                else {
                    fileList.push_back((*dirIter));
                }
            }

            ++dirIter;
        }
    }

    void MissionRecord::addFile(lindenb::io::Tar& archive, path filepath) {
        // relative would do what we want here, but it wasn't
        // introduced until boost 1.60, and we still want to support operating
        // systems with older versions.
        path abspath = absolute(filepath);
        path tempdirpath = absolute(this->temp_dir);
        path::iterator it_file = abspath.begin();
        path::iterator it_tmpdir = tempdirpath.begin();
        path relpath =
            this->mission_id; // Start with the mission_id as our root.
        // Skip everything which is in both paths:
        while (*it_file == *it_tmpdir && it_file != abspath.end() &&
               it_tmpdir != tempdirpath.end()) {
            it_file++, it_tmpdir++;
        }
        // Now get rest of file path:
        for (; it_file != abspath.end(); it_file++) {
            relpath /= *it_file;
        }
        string file_name_in_archive = relpath.normalize().string();
        replace(file_name_in_archive.begin(),
                file_name_in_archive.end(),
                '\\',
                '/');
        LOGINFO(LT("- adding "),
                filepath.string(),
                LT(" as "),
                file_name_in_archive);
        archive.putFile(filepath.string().c_str(),
                        file_name_in_archive.c_str());
    }

    bool MissionRecord::isRecording() const { return this->spec.isRecording(); }

    bool MissionRecord::isRecordingObservations() const {
        return this->spec.is_recording_observations;
    }

    bool MissionRecord::isRecordingRewards() const {
        return this->spec.is_recording_rewards;
    }

    bool MissionRecord::isRecordingCommands() const {
        return this->spec.is_recording_commands;
    }

    string MissionRecord::getMP4Path() const { return this->mp4_path; }

    string MissionRecord::getMP4DepthPath() const {
        return this->mp4_depth_path;
    }

    string MissionRecord::getMP4LuminancePath() const {
        return this->mp4_luminance_path;
    }

    string MissionRecord::getMP4ColourMapPath() const {
        return this->mp4_colourmap_path;
    }

    int64_t
    MissionRecord::getMP4BitRate(TimestampedVideoFrame::FrameType type) const {
        auto it = this->spec.video_recordings.find(type);
        return (it != this->spec.video_recordings.end())
                   ? it->second.mp4_bit_rate
                   : 0;
    }

    int MissionRecord::getMP4FramesPerSecond(
        TimestampedVideoFrame::FrameType type) const {
        auto it = this->spec.video_recordings.find(type);
        return (it != this->spec.video_recordings.end()) ? it->second.mp4_fps
                                                         : 0;
    }

    bool MissionRecord::isDroppingFrames(
        TimestampedVideoFrame::FrameType type) const {
        auto it = this->spec.video_recordings.find(type);
        return it == this->spec.video_recordings.end() ||
               it->second.drop_input_frames;
    }

    bool
    MissionRecord::isRecordingMP4(TimestampedVideoFrame::FrameType type) const {
        auto it = this->spec.video_recordings.find(type);
        return it != this->spec.video_recordings.end() &&
               it->second.fr_type == MissionRecordSpec::VIDEO;
    }

    bool MissionRecord::isRecordingBmps(
        TimestampedVideoFrame::FrameType type) const {
        auto it = this->spec.video_recordings.find(type);
        return it != this->spec.video_recordings.end() &&
               it->second.fr_type == MissionRecordSpec::BMP;
    }

    string MissionRecord::getObservationsPath() const {
        return this->observations_path;
    }

    string MissionRecord::getRewardsPath() const { return this->rewards_path; }

    string MissionRecord::getCommandsPath() const {
        return this->commands_path;
    }

    string MissionRecord::getMissionInitPath() const {
        return this->mission_init_path;
    }

    string MissionRecord::getMissionEndedPath() const {
        return this->mission_ended_path;
    }

    string MissionRecord::getTemporaryDirectory() const {
        if (!this->spec.isRecording()) {
            throw runtime_error("Mission is not being recorded.");
        }

        if (exists(this->temp_dir)) {
            return this->temp_dir.string();
        }
        else {
            throw runtime_error("Mission record does not yet exist. Temporary "
                                "directory will be "
                                "created once a mission has begun.");
        }
    }
} // namespace malmo

#undef LOG_COMPONENT
