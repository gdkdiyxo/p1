package ojt.management.business.services;

import ojt.management.common.exceptions.*;
import ojt.management.common.payload.request.JobCreateRequest;
import ojt.management.common.payload.request.JobRequest;
import ojt.management.data.entities.*;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.JobRepository;
import ojt.management.data.repositories.MajorRepository;
import ojt.management.data.repositories.SemesterRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Service
public class JobServiceImpl implements JobService {

    private final JobRepository jobRepository;
    private final SemesterRepository semesterRepository;
    private final MajorRepository majorRepository;
    private final AccountRepository accountRepository;

    public JobServiceImpl(JobRepository jobRepository,
                          SemesterRepository semesterRepository,
                          MajorRepository majorRepository,
                          AccountRepository accountRepository) {
        this.jobRepository = jobRepository;
        this.semesterRepository = semesterRepository;
        this.majorRepository = majorRepository;
        this.accountRepository = accountRepository;
    }

    @Override
    public Page<Job> searchJob(Specification<Job> specification, Pageable pageable) {
        return jobRepository.findAll(specification, pageable);
    }

    @Override
    public Job getById(Long id, Long accountId) throws JobNotExistedException {
        Account account = accountRepository.getById(accountId);
        if (account.getRepresentative() != null) { //The Rep only get their own job
            Long repCompanyId = accountRepository.getById(accountId).getRepresentative().getCompany().getId();
            if (jobRepository.getJobByRep(repCompanyId, id) == null) {
                throw new JobNotExistedException();
            }
            return jobRepository.getJobByRep(repCompanyId, id);
        } else { //The other role can get all
            if (Boolean.FALSE.equals(jobRepository.existsById(id))) {
                throw new JobNotExistedException();
            }
            return jobRepository.getById(id);
        }
    }

    @Override
    public Job updateJob(Long idJob, JobRequest jobUpdateRequest, Long accountId) throws CrudException {
        if (!jobRepository.existsById(idJob)) {
            throw new JobNotExistedException();
        }
        //Check authen: the Rep only can edit their own job
        Account account = accountRepository.getById(accountId);
        Long oldJob = jobRepository.getById(idJob).getCompany().getId();
        if (!account.isAdmin() || (!oldJob.equals(account.getRepresentative().getCompany().getId()))) {
            throw new JobNotAllowedUpdateException();
        }

        validateSemesterIdsAndMajorIds(jobUpdateRequest);

        Job job = jobRepository.getById(idJob);
        job.setName(jobUpdateRequest.getName());
        job.setDescription(jobUpdateRequest.getDescription());
        job.setTitle(jobUpdateRequest.getTitle());
        job.setBenefits(jobUpdateRequest.getBenefits());

        List<Long> newSemesterIds = jobUpdateRequest.getSemesterIds().stream()
                .filter(id -> !job.getSemesters().stream()
                        .map(Semester::getId).collect(Collectors.toList()).contains(id)).collect(Collectors.toList());
        job.getSemesters().addAll(newSemesterIds.stream().map(Semester::new).collect(Collectors.toList()));

        List<Long> newMajorIds = jobUpdateRequest.getMajorIds().stream()
                .filter(id -> !job.getMajors().stream()
                        .map(Major::getId).collect(Collectors.toList()).contains(id)).collect(Collectors.toList());
        job.getMajors().addAll(newMajorIds.stream().map(Major::new).collect(Collectors.toList()));

        return jobRepository.save(job);
    }


    @Override
    public boolean deleteJob(Long id, Long accountId) throws JobNotExistedException {
        //Check authen: the Rep only can delete their own job
        Long currentJob = jobRepository.getById(id).getId();
        Long repCompanyId = accountRepository.getById(accountId).getRepresentative().getCompany().getId();

        if (currentJob.equals(repCompanyId)) {
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
        return false;
    }

    @Override
    public Job createJob(JobCreateRequest jobCreateRequest, Long accountId) throws CrudException {
        validateSemesterIdsAndMajorIds(jobCreateRequest);
        Account account = accountRepository.getById(accountId);
        // create new job
        Job job = new Job();
        job.setName(jobCreateRequest.getName());
        job.setDescription(jobCreateRequest.getDescription());
        job.setTitle(jobCreateRequest.getTitle());
        job.setBenefits(jobCreateRequest.getBenefits());

        //Get company id of Rep
        if (account.getRepresentative() != null) {
            Long companyId = accountRepository.getById(accountId).getRepresentative().getCompany().getId();
            job.setCompany(new Company(companyId));
        } else if (account.isAdmin()) {
            job.setCompany(new Company(jobCreateRequest.getCompanyId()));
        }

        job = jobRepository.save(job);
        List<Semester> semesters = semesterRepository.findAllByIdIn(jobCreateRequest.getSemesterIds());
        List<Major> majors = majorRepository.findAllByIdIn(jobCreateRequest.getMajorIds());
        AtomicReference<Job> jobAtomicReference = new AtomicReference<>(job);
        semesters.forEach(semester -> semester.getJobs().add(jobAtomicReference.get()));
        majors.forEach(major -> major.getJobs().add(jobAtomicReference.get()));
        job.setSemesters(semesters.stream().collect(Collectors.toSet()));
        job.setMajors(majors.stream().collect(Collectors.toSet()));
        return jobRepository.save(job);
    }

    private void validateSemesterIdsAndMajorIds(JobRequest jobRequest) throws CrudException {
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
