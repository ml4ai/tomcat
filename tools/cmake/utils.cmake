macro(COPY_FILE_IF_CHANGED in_file out_file target)
    IF(${in_file} IS_NEWER_THAN ${out_file})
    #    message("COpying file: ${in_file} to: ${out_file}")
    	ADD_CUSTOM_COMMAND (
    		TARGET     ${target}
    		POST_BUILD
    		COMMAND    ${CMAKE_COMMAND}
    		ARGS       -E copy ${in_file} ${out_file}
    	)
	endIF(${in_file} IS_NEWER_THAN ${out_file})
endmacro(COPY_FILE_IF_CHANGED)

macro(COPY_FILE_INTO_DIRECTORY_IF_CHANGED in_file out_dir target)
	GET_FILENAME_COMPONENT(file_name ${in_file} NAME)
	COPY_FILE_IF_CHANGED(${in_file} ${out_dir}/${file_name}
${target})
endmacro(COPY_FILE_INTO_DIRECTORY_IF_CHANGED)

#Copies all the files from in_file_list into the out_dir.
# sub-trees are ignored (files are stored in same out_dir)
macro(COPY_FILES_INTO_DIRECTORY_IF_CHANGED in_file_list out_dir target)
    FOREACH(in_file ${in_file_list})
		COPY_FILE_INTO_DIRECTORY_IF_CHANGED(${in_file}
${out_dir} ${target})
	endFOREACH(in_file)
endmacro(COPY_FILES_INTO_DIRECTORY_IF_CHANGED)

#Copy all files and directories in in_dir to out_dir.
# Subtrees remain intact.
macro(COPY_DIRECTORY_IF_CHANGED in_dir out_dir target)
    #message("Copying directory ${in_dir}")
    FILE(GLOB_RECURSE in_file_list ${in_dir}/*)
	FOREACH(in_file ${in_file_list})
	    if(NOT ${in_file} MATCHES ".*/CVS.*")
    		STRING(REGEX REPLACE ${in_dir} ${out_dir} out_file
${in_file})
    		COPY_FILE_IF_CHANGED(${in_file} ${out_file} ${target})
    	endif(NOT ${in_file} MATCHES ".*/CVS.*")
	endFOREACH(in_file)
endmacro(COPY_DIRECTORY_IF_CHANGED)

function(download_file url filename)
  if(NOT EXISTS ${filename})
    file(DOWNLOAD ${url} ${filename} TIMEOUT 60 )
  endif()
endfunction(download_file)
