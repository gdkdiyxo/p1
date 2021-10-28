package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.ApplicationService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.ApplicationNotExistedException;
import ojt.management.common.exceptions.NotPermissionException;
import ojt.management.common.payload.dto.ApplicationDTO;
import ojt.management.common.payload.request.ApplicationCreateRequest;
import ojt.management.common.payload.request.ApplicationUpdateRequest;
import ojt.management.configuration.security.services.UserDetailsImpl;
import ojt.management.mappers.ApplicationMapper;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/applications")
@SecurityRequirement(name = "bearerAuth")
public class ApplicationController {

    private final ApplicationService applicationService;
    private final ApplicationMapper applicationMapper;

    public ApplicationController(ApplicationService applicationService,
                                 ApplicationMapper applicationMapper) {
        this.applicationService = applicationService;
        this.applicationMapper = applicationMapper;
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE', 'STUDENT')")
    @GetMapping()
    public List<ApplicationDTO> searchApplication(Authentication authentication)
            throws AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return applicationService.searchApplication(accountId).stream().map(applicationMapper::applicationToApplicationDTO)
                .collect(Collectors.toList());
    }

    @PreAuthorize("hasAnyAuthority('SYS_ADMIN', 'COMPANY_REPRESENTATIVE', 'STUDENT')")
    @GetMapping("/{id}")
    public ApplicationDTO getAppById(@PathVariable Long id,
                                     Authentication authentication)
            throws ApplicationNotExistedException, AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return applicationMapper.applicationToApplicationDTO(applicationService.getAppById(id, accountId));
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE', 'STUDENT')")
    @PutMapping("/{id}")
    public ApplicationDTO updateApplication(@Valid @RequestBody ApplicationUpdateRequest applicationUpdateRequest,
                                            @PathVariable Long id,
                                            Authentication authentication)
            throws ApplicationNotExistedException, NotPermissionException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return applicationMapper.applicationToApplicationDTO(
                applicationService.updateApplication(id, applicationUpdateRequest, accountId));
    }

    @PreAuthorize("hasAnyAuthority('STUDENT')")
    @DeleteMapping("/{id}")
    public boolean deleteApplication(@PathVariable Long id,
                                     Authentication authentication)
            throws ApplicationNotExistedException, NotPermissionException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return applicationService.deleteApplication(id, accountId);
    }

    @PreAuthorize("hasAnyAuthority('STUDENT')")
    @PostMapping()
    public ApplicationDTO createApplication(@Valid @RequestBody ApplicationCreateRequest applicationCreateRequest,
                                            Authentication authentication) {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return applicationMapper.applicationToApplicationDTO(
                applicationService.createApplication(applicationCreateRequest, accountId));
    }
}
