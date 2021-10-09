package ojt.management.business.services;

import ojt.management.common.exceptions.*;
import ojt.management.common.payload.request.JobRequest;
import ojt.management.common.payload.request.JobUpdateRequest;
import ojt.management.data.entities.Company;
import ojt.management.data.entities.Job;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;
import ojt.management.data.repositories.*;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class JobServiceImpl implements JobService {

    private final JobRepository jobRepository;
    private final AccountRepository accountRepository;
    private final SemesterRepository semesterRepository;
    private final MajorRepository majorRepository;
    private final CompanyRepository companyRepository;

    public JobServiceImpl(JobRepository jobRepository, AccountRepository accountRepository, SemesterRepository semesterRepository, MajorRepository majorRepository, CompanyRepository companyRepository) {
        this.jobRepository = jobRepository;
        this.accountRepository = accountRepository;
        this.semesterRepository = semesterRepository;
        this.majorRepository = majorRepository;
        this.companyRepository = companyRepository;
    }

    @Override
    public List<Job> searchJobs(String name, String title, Long semesterId, Long majorId) {
        if (name == null && title == null && semesterId == null && majorId == null) {
            return jobRepository.findAll();
        } else {
            return jobRepository.searchJob(
                    Optional.ofNullable(name).orElse(""),
                    Optional.ofNullable(title).orElse(""),
                    semesterId,
                    majorId);
        }
    }

    @Override
    public Job getById(Long id) throws JobNotExistedException {
        if (Boolean.FALSE.equals(jobRepository.existsById(id))) {
            throw new JobNotExistedException();
        } else {
            return jobRepository.getById(id);
        }
    }

    @Override
    public Job updateJob(JobUpdateRequest jobUpdateRequest) throws CrudException {
        if (!jobRepository.existsById(jobUpdateRequest.getId())) {
            throw new JobNotExistedException();
        }

        validateCompanyIdAndSemesterIdsAndMajorIds(jobUpdateRequest);

        Job job = jobRepository.getById(jobUpdateRequest.getId());
        job.setName(jobUpdateRequest.getName());
        job.setDescription(jobUpdateRequest.getDescription());
        job.setTitle(jobUpdateRequest.getTitle());
        job.setCompany(new Company(jobUpdateRequest.getCompanyId()));
        List<Long> newSemesterIds = jobUpdateRequest.getSemesterIds().stream()
                .filter(id -> !job.getSemesters().stream()
                        .map(Semester::getId).collect(Collectors.toList()).contains(id)).collect(Collectors.toList());
        job.getSemesters().addAll(newSemesterIds.stream().map(id -> new Semester(id)).collect(Collectors.toList()));
        List<Long> newMajorIds = jobUpdateRequest.getMajorIds().stream()
                .filter(id -> !job.getMajors().stream()
                        .map(Major::getId).collect(Collectors.toList()).contains(id)).collect(Collectors.toList());
        job.getMajors().addAll(newMajorIds.stream().map(id -> new Major(id)).collect(Collectors.toList()));
        return jobRepository.save(job);
    }


    @Override
    public boolean deleteJob(Long id) throws JobNotExistedException {
        if (Boolean.FALSE.equals(jobRepository.existsById(id))) {
            throw new JobNotExistedException();
        } else {
            Job job = jobRepository.getById(id);
            if (!job.isDisabled()) {
                job.setDisabled(true);
                jobRepository.save(job);
            }
            return true;
        }
    }

    @Override
    public Job createJob(JobRequest jobCreateRequest) throws CrudException {
        validateCompanyIdAndSemesterIdsAndMajorIds(jobCreateRequest);

        // create new job
        Job job = new Job();
        job.setName(jobCreateRequest.getName());
        job.setDescription(jobCreateRequest.getDescription());
        job.setTitle(jobCreateRequest.getTitle());
        job.setCompany(new Company(jobCreateRequest.getCompanyId()));
        job.setSemesters(jobCreateRequest.getSemesterIds().stream().map(semesterId -> new Semester(semesterId))
                .collect(Collectors.toSet()));
        job.setMajors(jobCreateRequest.getMajorIds().stream().map(majorId -> new Major(majorId))
                .collect(Collectors.toSet()));
        return jobRepository.save(job);
    }

    private void validateCompanyIdAndSemesterIdsAndMajorIds(JobRequest jobRequest) throws CrudException {
        // check if company id exist.
        if (!companyRepository.existsById(jobRequest.getCompanyId())) {
            throw new CompanyNotExistedException();
        }

        // check if semester ids exist.
        List<Long> semesterIds = semesterRepository.findAll()
                .stream().map(Semester::getId).collect(Collectors.toList());
        if (jobRequest.getSemesterIds().stream().anyMatch(id -> !semesterIds.contains(id))) {
            throw new SemesterNotExistedException();
        }

        // check if major ids exist.
        List<Long> majorIds = majorRepository.findAll()
                .stream().map(Major::getId).collect(Collectors.toList());
        if (jobRequest.getMajorIds().stream().anyMatch(id -> !majorIds.contains(id))) {
            throw new MajorNotExistedException();
        }
    }

}
