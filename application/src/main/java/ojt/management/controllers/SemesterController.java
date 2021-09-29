package ojt.management.controllers;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.SemesterService;
import ojt.management.common.payload.dto.SemesterDTO;
import ojt.management.common.payload.request.SemesterCreateRequest;
import ojt.management.common.payload.request.SemesterUpdateRequest;
import ojt.management.data.entities.Semester;
import ojt.management.mappers.SemesterMapper;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/semesters")
@SecurityRequirement(name = "bearerAuth")
public class SemesterController {

    private final SemesterService semesterService;
    private final SemesterMapper semesterMapper;

    public SemesterController(SemesterService semesterService, SemesterMapper semesterMapper) {
        this.semesterService = semesterService;
        this.semesterMapper = semesterMapper;
    }

    @GetMapping("/{id}")
    public SemesterDTO getById(@PathVariable Long id) {
        return semesterMapper.semesterToSemesterDTO(semesterService.getById(id));
    }

    @GetMapping()
    public List<SemesterDTO> searchSemesters(@RequestParam (value = "name", required = false) String name,
                                             @RequestParam (value = "startDate", required = false) Date startDate,
                                             @RequestParam (value = "endDate", required = false) Date endDate) {
        return semesterService.searchSemesters(name, startDate, endDate).stream().map(semesterMapper::semesterToSemesterDTO).collect(Collectors.toList());
    }

    @PutMapping("/{id}")
    public SemesterDTO updateSemester(@Valid @RequestBody SemesterUpdateRequest semesterUpdateRequest) {
        return semesterMapper.semesterToSemesterDTO(semesterService.updateSemester(semesterUpdateRequest.getId(),
                semesterUpdateRequest.getName(), semesterUpdateRequest.getStartDate(), semesterUpdateRequest.getEndDate()));
    }

    @DeleteMapping("/{id}")
    public boolean deleteSemester(@PathVariable Long id) {
        return semesterService.deleteSemester(id);
    }

    @PostMapping()
    public SemesterDTO createSemester(@Valid @RequestBody SemesterCreateRequest semesterCreateRequest) {
        return semesterMapper.semesterToSemesterDTO(semesterService.createSemester(semesterCreateRequest.getName(),
                semesterCreateRequest.getStartDate(), semesterCreateRequest.getEndDate()));
    }
}
