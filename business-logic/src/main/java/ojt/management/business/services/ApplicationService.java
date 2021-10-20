package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.ApplicationNotExistedException;
import ojt.management.common.payload.request.ApplicationRequest;
import ojt.management.data.entities.Application;

import java.util.List;

public interface ApplicationService {

    Application getAppById(Long id, Long accountId) throws ApplicationNotExistedException, AccountIdNotExistedException;

    List<Application> searchApplication(Long accountId) throws AccountIdNotExistedException;

    Application updateApplication(Long id, ApplicationRequest applicationRequest) throws ApplicationNotExistedException;

    boolean deleteApplication(Long id) throws ApplicationNotExistedException;

    Application createApplication(ApplicationRequest applicationRequest);
}
