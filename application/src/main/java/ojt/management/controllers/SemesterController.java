package ojt.management.controllers;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.SemesterService;
import ojt.management.common.exceptions.SemesterAlreadyExistedException;
import ojt.management.common.exceptions.SemesterNotExistedException;
import ojt.management.common.payload.dto.SemesterDTO;
import ojt.management.common.payload.request.SemesterCreateRequest;
import ojt.management.common.payload.request.SemesterUpdateRequest;
import ojt.management.data.repositories.SemesterRepository;
import ojt.management.mappers.SemesterMapper;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@PostAuthorize("hasAnyAuthority('SYS_ADMIN', 'STUDENT', 'COMPANY_REPRESENTATIVE')")
@RequestMapping("/semesters")
@SecurityRequirement(name = "bearerAuth")
public class SemesterController {

    private final SemesterService semesterService;
    private final SemesterMapper semesterMapper;
    private final SemesterRepository semesterRepository;

    public SemesterController(SemesterService semesterService,
                              SemesterMapper semesterMapper,
                              SemesterRepository semesterRepository) {
        this.semesterService = semesterService;
        this.semesterMapper = semesterMapper;
        this.semesterRepository = semesterRepository;
    }

    @GetMapping("/{id}")
    public SemesterDTO getById(@PathVariable Long id) throws SemesterNotExistedException {
        if (Boolean.FALSE.equals(semesterRepository.existsById(id))) {
            throw new SemesterNotExistedException();
        } else {
            return semesterMapper.semesterToSemesterDTO(semesterService.getById(id));
        }
    }

    @GetMapping()
    public List<SemesterDTO> searchSemesters(@RequestParam (value = "name", required = false) String name,
                                             @RequestParam (value = "startDate", required = false) Date startDate,
                                             @RequestParam (value = "endDate", required = false) Date endDate) {
        return semesterService.searchSemesters(name, startDate, endDate).stream().map(semesterMapper::semesterToSemesterDTO).collect(Collectors.toList());
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @PutMapping("/{id}")
    public SemesterDTO updateSemester(@Valid @RequestBody SemesterUpdateRequest semesterUpdateRequest)
            throws SemesterNotExistedException, SemesterAlreadyExistedException {
        if (Boolean.FALSE.equals(semesterRepository.existsById(semesterUpdateRequest.getId()))) {
            throw new SemesterNotExistedException();
        } else if (Boolean.TRUE.equals(semesterRepository.existsByNameAndAndStartDateAndEndDate(
                semesterUpdateRequest.getName(),
                semesterUpdateRequest.getStartDate(),
                semesterUpdateRequest.getEndDate()))) {
            throw new SemesterAlreadyExistedException();
        }else {
            return semesterMapper.semesterToSemesterDTO(semesterService.updateSemester(
                    semesterUpdateRequest.getId(),
                    semesterUpdateRequest.getName(),
                    semesterUpdateRequest.getStartDate(),
                    semesterUpdateRequest.getEndDate()));
        }
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @DeleteMapping("/{id}")
    public boolean deleteSemester(@PathVariable Long id) {
        return semesterService.deleteSemester(id);
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @PostMapping()
    public SemesterDTO createSemester(@Valid @RequestBody SemesterCreateRequest semesterCreateRequest)
            throws SemesterAlreadyExistedException {
        if (Boolean.TRUE.equals(semesterRepository.existsByNameAndAndStartDateAndEndDate(
                semesterCreateRequest.getName(),
                semesterCreateRequest.getStartDate(),
                semesterCreateRequest.getEndDate()))) {
            throw new SemesterAlreadyExistedException();
        } else {
            return semesterMapper.semesterToSemesterDTO(semesterService.createSemester(
                    semesterCreateRequest.getName(),
                    semesterCreateRequest.getStartDate(),
                    semesterCreateRequest.getEndDate()));
        }
    }
}
