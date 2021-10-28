package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.ApplicationNotExistedException;
import ojt.management.common.exceptions.NotPermissionException;
import ojt.management.common.payload.request.ApplicationCreateRequest;
import ojt.management.common.payload.request.ApplicationUpdateRequest;
import ojt.management.data.entities.Application;

import java.util.List;

public interface ApplicationService {

    Application getAppById(Long id, Long accountId) throws ApplicationNotExistedException, AccountIdNotExistedException;

    List<Application> searchApplication(Long accountId) throws AccountIdNotExistedException;

    Application updateApplication(Long id, ApplicationUpdateRequest applicationUpdateRequest, Long accountId)
            throws ApplicationNotExistedException, NotPermissionException;

    boolean deleteApplication(Long id, Long accountId) throws ApplicationNotExistedException, NotPermissionException;

    Application createApplication(ApplicationCreateRequest applicationCreateRequest, Long accountId);
}
