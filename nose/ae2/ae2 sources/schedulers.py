import math

from des import SchedulerDES
from event import Event, EventTypes
from process import ProcessStates

class FCFS(SchedulerDES):
    def scheduler_func(self, cur_event):
        return self.processes[cur_event.process_id]

    def dispatcher_func(self, cur_process):
        cur_process.process_state = ProcessStates.RUNNING
        time_ran = cur_process.run_for(cur_process.service_time, self.time)
        cur_process.process_state = ProcessStates.TERMINATED
        return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_DONE)


class SJF(SchedulerDES):

    def scheduler_func(self, cur_event):
        self.processes.sort(key = lambda x : x.service_time)
        for ind, val in enumerate(self.processes):
            if val.process_state == ProcessStates.READY:
                return self.processes[ind]

    def dispatcher_func(self, cur_process):
        cur_process.process_state = ProcessStates.RUNNING
        time_ran = cur_process.run_for(cur_process.service_time, self.time)
        cur_process.process_state = ProcessStates.TERMINATED
        return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_DONE)


class RR(SchedulerDES):
    def scheduler_func(self, cur_event):
        return self.processes[cur_event.process_id]

    def dispatcher_func(self, cur_process):
        cur_process.process_state = ProcessStates.RUNNING
        if cur_process.remaining_time < self.quantum:
            time_ran = cur_process.run_for(cur_process.remaining_time, self.time)
            cur_process.process_state = ProcessStates.TERMINATED
            return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_DONE)

        time_ran = cur_process.run_for(self.quantum, self.time)
        cur_process.process_state = ProcessStates.READY
        return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_REQ)


class SRTF(SchedulerDES):
    def scheduler_func(self, cur_event):
        self.processes.sort(key = lambda x : x.remaining_time + self.context_switch_time)
        for ind, val in enumerate(self.processes):
            if val.process_state == ProcessStates.READY:
                return self.processes[ind]

    def dispatcher_func(self, cur_process):
        cur_process.process_state = ProcessStates.RUNNING
        if self.next_event_time() < cur_process.remaining_time + self.time:
            time_ran = cur_process.run_for(self.next_event_time() - self.time, self.time)
        else:
            time_ran = cur_process.run_for(cur_process.remaining_time, self.time)

        if cur_process.remaining_time <= 0:
            cur_process.process_state = ProcessStates.TERMINATED
            return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_DONE)

        cur_process.process_state = ProcessStates.READY
        return Event(process_id = cur_process.process_id, event_time = self.time + time_ran, event_type = EventTypes.PROC_CPU_REQ)
