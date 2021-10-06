package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.MajorService;
import ojt.management.common.exceptions.MajorNameAlreadyExistedException;
import ojt.management.common.exceptions.MajorNotExistedException;
import ojt.management.common.payload.dto.MajorDTO;
import ojt.management.common.payload.request.MajorUpdateRequest;
import ojt.management.data.repositories.MajorRepository;
import ojt.management.mappers.MajorMapper;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@PostAuthorize("hasAnyAuthority('SYS_ADMIN', 'STUDENT')")
@RequestMapping("/majors")
@SecurityRequirement(name = "bearerAuth")
public class MajorController {

    private final MajorService majorService;
    private final MajorMapper majorMapper;

<<<<<<< HEAD
    public MajorController(MajorService majorService, MajorMapper majorMapper) {
=======
    public MajorController(MajorService majorService,
                           MajorMapper majorMapper,
                           MajorRepository majorRepository) {
>>>>>>> develop
        this.majorMapper = majorMapper;
        this.majorService = majorService;
    }

    @GetMapping("/{id}")
<<<<<<< HEAD
    public MajorDTO getMajorById(@PathVariable Long id) throws MajorNotExistedException {
        return majorMapper.majorToMajorDTO(majorService.getMajorById(id));
=======
    public MajorDTO getMajorById(@PathVariable Long id)
            throws MajorNotExistedException {
        if (Boolean.FALSE.equals(majorRepository.existsById(id))) {
            throw new MajorNotExistedException();
        } else {
            return majorMapper.majorToMajorDTO(majorService.getMajorById(id));
        }
>>>>>>> develop
    }

    @GetMapping()
    public List<MajorDTO> searchMajor(@RequestParam(value = "name", required = false) String name) {
        return majorService.searchMajor(name).stream().map(majorMapper::majorToMajorDTO).collect(Collectors.toList());
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @PutMapping("/{id}")
<<<<<<< HEAD
    public MajorDTO updateMajor(@Valid @RequestBody MajorUpdateRequest majorUpdateRequest) throws MajorNotExistedException, MajorNameAlreadyExistedException {
        return majorMapper.majorToMajorDTO(majorService.updateMajor(majorUpdateRequest.getId(), majorUpdateRequest.getName()));
=======
    public MajorDTO updateMajor(@Valid @RequestBody MajorUpdateRequest majorUpdateRequest)
            throws MajorNotExistedException,
            MajorNameAlreadyExistedException {
        if (Boolean.FALSE.equals(majorRepository.existsById(majorUpdateRequest.getId()))) {
            throw new MajorNotExistedException();
        } else if (Boolean.TRUE.equals(majorRepository.existsByName(majorUpdateRequest.getName()))) {
            throw new MajorNameAlreadyExistedException();
        } else {
            return majorMapper.majorToMajorDTO(majorService.updateMajor(majorUpdateRequest.getId(), majorUpdateRequest.getName()));
        }
>>>>>>> develop
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @DeleteMapping("/{id}")
    public boolean deleteMajor(@PathVariable Long id) throws MajorNotExistedException {
        return majorService.deleteMajor(id);
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @PostMapping()
<<<<<<< HEAD
    public MajorDTO createMajor(@RequestBody String name) throws MajorNameAlreadyExistedException {
        return majorMapper.majorToMajorDTO(majorService.createMajor(name));
=======
    public MajorDTO createMajor(@Valid @RequestBody @NotNull @NotBlank @Max(255) String name)
            throws MajorNameAlreadyExistedException {
        if (Boolean.TRUE.equals(majorRepository.existsByName(name))) {
            throw new MajorNameAlreadyExistedException();
        } else {
            return majorMapper.majorToMajorDTO(majorService.createMajor(name));
        }
>>>>>>> develop
    }
}
