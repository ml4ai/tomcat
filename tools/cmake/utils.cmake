macro(COPY_FILE_IF_CHANGED
      in_file
      out_file
      target)
  if(${in_file} IS_NEWER_THAN ${out_file})
    # message("COpying file: ${in_file} to: ${out_file}")
    add_custom_command(TARGET ${target} POST_BUILD
                       COMMAND ${CMAKE_COMMAND}
                               ARGS
                               -E
                               copy
                               ${in_file}
                               ${out_file})
  endif(${in_file} IS_NEWER_THAN ${out_file})
endmacro(COPY_FILE_IF_CHANGED)

macro(COPY_FILE_INTO_DIRECTORY_IF_CHANGED
      in_file
      out_dir
      target)
  get_filename_component(file_name ${in_file} NAME)
  copy_file_if_changed(${in_file} ${out_dir}/${file_name} ${target})
endmacro(COPY_FILE_INTO_DIRECTORY_IF_CHANGED)

# Copies all the files from in_file_list into the out_dir. sub-trees are ignored
# (files are stored in same out_dir)
macro(COPY_FILES_INTO_DIRECTORY_IF_CHANGED
      in_file_list
      out_dir
      target)
  foreach(in_file ${in_file_list})
    copy_file_into_directory_if_changed(${in_file} ${out_dir} ${target})
  endforeach(in_file)
endmacro(COPY_FILES_INTO_DIRECTORY_IF_CHANGED)

# Copy all files and directories in in_dir to out_dir. Subtrees remain intact.
macro(COPY_DIRECTORY_IF_CHANGED
      in_dir
      out_dir
      target)
  # message("Copying directory ${in_dir}")
  file(GLOB_RECURSE in_file_list ${in_dir}/*)
  foreach(in_file ${in_file_list})
    if(NOT ${in_file} MATCHES ".*/CVS.*")
      string(REGEX
             REPLACE ${in_dir}
                     ${out_dir}
                     out_file
                     ${in_file})
      copy_file_if_changed(${in_file} ${out_file} ${target})
    endif(NOT ${in_file} MATCHES ".*/CVS.*")
  endforeach(in_file)
endmacro(COPY_DIRECTORY_IF_CHANGED)

function(download_and_unzip_tgz
         url
         filename
         file_description)
  if(NOT EXISTS ${filename})
    message("Downloading ${file_description}...")
    file(DOWNLOAD ${url} ${filename} TIMEOUT 60)
    execute_process(COMMAND ${CMAKE_COMMAND}
                            -E
                            tar
                            -xvzf
                            ${filename}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  endif()
endfunction()
