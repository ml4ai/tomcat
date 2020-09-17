#include "MessageConverter.h"

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        MessageConverter::MessageConverter() {}

        MessageConverter::MessageConverter(int time_gap) : time_gap(time_gap) {}

        MessageConverter::~MessageConverter() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        MessageConverter::copy_converter(const MessageConverter& converter) {
            this->time_gap = converter.time_gap;
            this->time_step = converter.time_step;
        }

    } // namespace model
} // namespace tomcat
