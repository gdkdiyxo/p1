package ojt.management.business.services;

import ojt.management.data.entities.Job;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;

import java.util.List;
import java.util.Set;

public interface JobService {

    List<Job> searchJobs(String name, String description, String title, Set<Semester> semesters, Set<Major> major);

    Job getById(Long id);

    Job updateJob(Long id, String name, String description, String title, Set<Semester> semesters, Set<Major> majors);

    boolean deleteJob(Long id);

    Job createJob(String name, String description, String title, Set<Semester> semesters, Set<Major> majors);
}
