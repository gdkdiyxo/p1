package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.ApplicationNotExistedException;
import ojt.management.common.payload.request.ApplicationRequest;
import ojt.management.data.entities.Account;
import ojt.management.data.entities.Application;
import ojt.management.data.entities.Job;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.ApplicationRepository;
import ojt.management.data.repositories.JobRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ApplicationServiceImpl implements ApplicationService {

    private final ApplicationRepository applicationRepository;
    private final JobRepository jobRepository;
    private final AccountRepository accountRepository;

    public ApplicationServiceImpl(ApplicationRepository applicationRepository,
                                  JobRepository jobRepository,
                                  AccountRepository accountRepository) {
        this.applicationRepository = applicationRepository;
        this.jobRepository = jobRepository;
        this.accountRepository = accountRepository;
    }

    @Override
    public List<Application> searchApplication(Long accountId)
            throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(accountId))) {
            throw new AccountIdNotExistedException();
        }
        Account account = accountRepository.getById(accountId);
        if (account.getRepresentative() != null) {
            return applicationRepository.searchAppRep(account.getRepresentative().getCompany().getId());
        } else {
            return applicationRepository.searchAppStudent(account.getStudent().getId());
        }
    }

    @Override
    public Application getAppById(Long id, Long accountId)
            throws ApplicationNotExistedException, AccountIdNotExistedException {
        if (Boolean.FALSE.equals(applicationRepository.existsById(id))) {
            throw new ApplicationNotExistedException();
        }
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        }
        Account account = accountRepository.getById(accountId);
        if (account.getRepresentative() != null) {
            return applicationRepository.getAppRep(id, account.getRepresentative().getCompany().getId());
        } else {
            return applicationRepository.getAppStudent(id, account.getStudent().getId());
        }
    }

    @Override
    public boolean deleteApplication(Long id)
            throws ApplicationNotExistedException {
        if (Boolean.FALSE.equals(applicationRepository.existsById(id))) {
            throw new ApplicationNotExistedException();
        }
        Application application = applicationRepository.getById(id);
        if (!application.isDisabled()) {
            application.setDisabled(true);
            applicationRepository.save(application);
            return true;
        }
        return false;
    }

    @Override
    public Application updateApplication(Long id, ApplicationRequest applicationRequest)
            throws ApplicationNotExistedException {
        if (Boolean.FALSE.equals(applicationRepository.existsById(id))) {
            throw new ApplicationNotExistedException();
        }
        Application application = applicationRepository.getById(id);
        if (application.isDisabled()) {
            throw new ApplicationNotExistedException();
        } else {
            if (!applicationRequest.getExperience().isEmpty()) 
                application.setExperience(applicationRequest.getExperience());
            application.setCompanyAccepted(applicationRequest.isCompanyAccepted());
            application.setStudentConfirmed(applicationRequest.isStudentConfirmed());
            applicationRepository.save(application);
            return application;
        }
    }

    @Override
    public Application createApplication(ApplicationRequest applicationRequest) {
        Account account = accountRepository.getById(applicationRequest.getAccountId());
        Job job = jobRepository.getById(applicationRequest.getJobId());
        Application application = new Application();
        application.setExperience(applicationRequest.getExperience());
        application.setJob(job);
        application.setStudent(account.getStudent());
        application.setStudentConfirmed(false);
        application.setCompanyAccepted(false);
        applicationRepository.save(application);
        return application;
    }
}
