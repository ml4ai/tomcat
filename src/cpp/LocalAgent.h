#pragma once

namespace tomcat {

    class Mission; // Forward declaration to deal with circular dependency

    /**
     * The LocalAgent class is a template for ToMCAT agents that accompany human
     * players.
     */
    class LocalAgent {

      public:
        /**
         * Constructor
         * */
        LocalAgent();

        /**
         * Destructor
         * */
        ~LocalAgent();

        /**
         * Method called by a mission main loop
         * @param mission - Tomcat mission
         */
        void observe_mission(Mission& mission);
    };

} // namespace tomcat
