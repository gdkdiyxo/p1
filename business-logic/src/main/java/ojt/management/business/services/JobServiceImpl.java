package ojt.management.business.services;
import ojt.management.common.payload.request.LoginRequest;
import ojt.management.data.entities.*;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.JobRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public class JobServiceImpl implements JobService{

    private final JobRepository jobRepository;
    private final AccountRepository accountRepository;

    public JobServiceImpl(JobRepository jobRepository, AccountRepository accountRepository) {
        this.jobRepository = jobRepository;
        this.accountRepository = accountRepository;
    }

    @Override
    public List<Job> searchJobs(String name, String description, String title, Set<Semester> semesters, Set<Major> majors) {
        if (name == null & description == null & title == null & semesters == null & majors == null) {
            return jobRepository.findAll();
        } else {
            return jobRepository.searchJob(name, description, title, semesters, majors);
        }
    }

    @Override
    public Job getById(Long id) {
        return jobRepository.getById(id);
    }

    @Override
    public Job updateJob(Long id, String name, String description, String title, Set<Semester> semesters, Set<Major> majors) {
        Job job = jobRepository.getById(id);
        if (name != null)
            job.setName(name);
        if (description != null)
            job.setDescription(description);
        if (title != null)
            job.setTitle(title);
        if (semesters != null)
            job.setSemesters(semesters);
        if (majors != null)
            job.setMajors(majors);
        jobRepository.save(job);
        return jobRepository.getById(id);
    }

    @Override
    public boolean deleteJob(Long id) {
        boolean response = false;
        Job job = jobRepository.getById(id);
        if (job != null) {
            job.setDisabled(true);
            response = true;
            return response;
        } else
            return response;
    }

    @Override
    public Job createJob(String name, String description, String title, Set<Semester> semesters, Set<Major> majors) {
        LoginRequest loginRequest = new LoginRequest();
        Account account = accountRepository.findByEmail(loginRequest.getEmail());
        Company company = account.getRepresentative().getCompany();
        Job job = new Job(name, description, title);
        job.setSemesters(semesters);
        job.setMajors(majors);
        job.setCompany(company);
        jobRepository.save(job);
        return jobRepository.getById(job.getId());
    }

}
