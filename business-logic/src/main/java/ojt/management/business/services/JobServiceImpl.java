package ojt.management.business.services;

import ojt.management.common.exceptions.*;
import ojt.management.common.payload.request.JobRequest;
import ojt.management.common.payload.request.JobUpdateRequest;
import ojt.management.configuration.security.services.UserDetailsImpl;
import ojt.management.data.entities.Account;
import ojt.management.data.entities.Company;
import ojt.management.data.entities.Job;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;
import ojt.management.data.repositories.*;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class JobServiceImpl implements JobService {

    private final JobRepository jobRepository;
    private final SemesterRepository semesterRepository;
    private final MajorRepository majorRepository;
    private final CompanyRepository companyRepository;
    private final RepresentativeRepository representativeRepository;
    private final AccountRepository accountRepository;

    public JobServiceImpl(JobRepository jobRepository,
                          SemesterRepository semesterRepository,
                          MajorRepository majorRepository,
                          CompanyRepository companyRepository,
                          RepresentativeRepository representativeRepository, AccountRepository accountRepository) {
        this.jobRepository = jobRepository;
        this.semesterRepository = semesterRepository;
        this.majorRepository = majorRepository;
        this.companyRepository = companyRepository;
        this.representativeRepository = representativeRepository;
        this.accountRepository = accountRepository;
    }

    @Override
    public List<Job> searchJobs(String name, String title, Long semesterId, Long majorId, Authentication authentication) {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        String accountRole = ((UserDetailsImpl) authentication.getPrincipal()).getAuthorities().toString();
        Long repCompanyId = representativeRepository.companyId(accountId);
        if (accountRole == "COMPANY_REPRESENTATIVE") { //The Rep only get their own job
            return jobRepository.searchJobByRep(Optional.ofNullable(name).orElse(""),
                    Optional.ofNullable(title).orElse(""),
                    semesterId,
                    majorId,
                    repCompanyId);
        } else {
            return jobRepository.searchJob(
                    Optional.ofNullable(name).orElse(""),
                    Optional.ofNullable(title).orElse(""),
                    semesterId,
                    majorId);
        }
    }

    @Override
    public Job getById(Long id, Authentication authentication) throws JobNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        String accountRole = ((UserDetailsImpl) authentication.getPrincipal()).getAuthorities().toString();
        if (accountRole == "COMPANY_REPRESENTATIVE") { //The Rep only get their own job
            Long repCompanyId = representativeRepository.companyId(accountId);
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
    public Job updateJob(JobUpdateRequest jobUpdateRequest, Authentication authentication) throws CrudException {
        if (!jobRepository.existsById(jobUpdateRequest.getId())) {
            throw new JobNotExistedException();
        }
        //Check authen: the Rep only can edit their own job
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        Account account = accountRepository.getById(accountId);
        Job oldJob = jobRepository.getById(jobUpdateRequest.getId());

        if(account.isAdmin() || (account.getRepresentative().getCompany().getId() == oldJob.getCompany().getId())) {

            validateCompanyIdAndSemesterIdsAndMajorIds(jobUpdateRequest);

            Job job = jobRepository.getById(jobUpdateRequest.getId());
            job.setName(jobUpdateRequest.getName());
            job.setDescription(jobUpdateRequest.getDescription());
            job.setTitle(jobUpdateRequest.getTitle());
            job.setCompany(new Company(jobUpdateRequest.getCompanyId()));
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
        return null;
    }


    @Override
    public boolean deleteJob(Long id, Authentication authentication) throws JobNotExistedException {
        //Check authen: the Rep only can delete their own job
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        Long jobCompanyId = jobRepository.companyId(id);
        Long repCompanyId = representativeRepository.companyId(accountId);
        if (repCompanyId == jobCompanyId) {
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
    public Job createJob(JobRequest jobCreateRequest) throws CrudException {
        validateCompanyIdAndSemesterIdsAndMajorIds(jobCreateRequest);

        // create new job
        Job job = new Job();
        job.setName(jobCreateRequest.getName());
        job.setDescription(jobCreateRequest.getDescription());
        job.setTitle(jobCreateRequest.getTitle());
        job.setCompany(new Company(jobCreateRequest.getCompanyId()));
        job.setSemesters(jobCreateRequest.getSemesterIds().stream().map(Semester::new)
                .collect(Collectors.toSet()));
        job.setMajors(jobCreateRequest.getMajorIds().stream().map(Major::new)
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
