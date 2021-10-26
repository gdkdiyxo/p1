package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.ApplicationService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.ApplicationNotExistedException;
import ojt.management.common.payload.dto.ApplicationDTO;
import ojt.management.common.payload.request.ApplicationRequest;
import ojt.management.mappers.ApplicationMapper;
import org.springframework.security.access.prepost.PreAuthorize;
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
    public List<ApplicationDTO> searchApplication(@RequestParam(value = "accountId", required = true) Long accountId)
            throws AccountIdNotExistedException {
        return applicationService.searchApplication(accountId).stream().map(applicationMapper::applicationToApplicationDTO)
                .collect(Collectors.toList());
    }

    @PreAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @GetMapping("/{id}")
    public ApplicationDTO getAppById(@PathVariable Long id,
                                     @RequestParam(value = "accountId", required = true) Long accountId)
            throws ApplicationNotExistedException, AccountIdNotExistedException {
        return applicationMapper.applicationToApplicationDTO(applicationService.getAppById(id, accountId));
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE', 'STUDENT')")
    @PutMapping("/{id}")
    public ApplicationDTO updateApplication(@Valid @RequestBody ApplicationRequest applicationRequest,
                                            @PathVariable Long id)
            throws ApplicationNotExistedException {
        return applicationMapper.applicationToApplicationDTO(applicationService.updateApplication(id, applicationRequest));
    }

    @PreAuthorize("hasAnyAuthority('STUDENT')")
    @DeleteMapping("/{id}")
    public boolean deleteApplication(@PathVariable Long id)
            throws ApplicationNotExistedException {
        return applicationService.deleteApplication(id);
    }

    @PreAuthorize("hasAnyAuthority('STUDENT')")
    @PostMapping()
    public ApplicationDTO createApplication(@Valid @RequestBody ApplicationRequest applicationRequest) {
        return applicationMapper.applicationToApplicationDTO(applicationService.createApplication(applicationRequest));
    }
}