package ojt.management.controllers;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.JobService;
import ojt.management.common.exceptions.CrudException;
import ojt.management.common.exceptions.JobNotExistedException;
import ojt.management.common.payload.dto.JobDTO;
import ojt.management.common.payload.request.JobRequest;
import ojt.management.common.payload.request.JobUpdateRequest;
import ojt.management.mappers.JobMapper;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/jobs")
@SecurityRequirement(name = "bearerAuth")
public class JobController {

    private final JobService jobService;
    private final JobMapper jobMapper;

    public JobController(JobMapper jobMapper, JobService jobService) {
        this.jobMapper = jobMapper;
        this.jobService = jobService;
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE','SYS_ADMIN', 'STUDENT')")
    @GetMapping()
    public List<JobDTO> searchJobs(@RequestParam(value = "name", required = false) String name,
                                   @RequestParam(value = "description", required = false) String description,
                                   @RequestParam(value = "title", required = false) String title,
                                   @RequestParam(value = "semesterId", required = false) Long semesterId,
                                   @RequestParam(value = "majorId", required = false) Long majorId) {
        return jobService.searchJobs(name, title, semesterId, majorId).stream().map(jobMapper::jobToJobDTO).collect(Collectors.toList());
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE','SYS_ADMIN', 'STUDENT')")
    @GetMapping("/{id}")
    public JobDTO getById(@PathVariable Long id) throws JobNotExistedException {
        return jobMapper.jobToJobDTO(jobService.getById(id));
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE','SYS_ADMIN')")
    @PutMapping("/{id}")
    public JobDTO updateJob(@Valid @RequestBody JobUpdateRequest jobUpdateRequest) throws CrudException {
        return jobMapper.jobToJobDTO(jobService.updateJob(jobUpdateRequest));
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE','SYS_ADMIN')")
    @DeleteMapping("/{id}")
    public boolean deleteJob(@PathVariable Long id) throws JobNotExistedException {
        return jobService.deleteJob(id);
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE',SYS_ADMIN)")
    @PostMapping
    public JobDTO createJob(@Valid @RequestBody JobRequest jobCreateRequest) throws CrudException {
        return jobMapper.jobToJobDTO(jobService.createJob(jobCreateRequest));
    }
}
