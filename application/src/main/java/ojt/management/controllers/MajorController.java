package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.MajorService;
import ojt.management.common.payload.dto.MajorDTO;
import ojt.management.mappers.MajorMapper;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/majors")
@SecurityRequirement(name = "bearerAuth")
public class MajorController {

    private final MajorService majorService;
    private final MajorMapper majorMapper;

    public MajorController(MajorService majorSevice, MajorMapper majorMapper) {
        this.majorMapper = majorMapper;
        this.majorService = majorSevice;
    }

    @GetMapping
    public List<MajorDTO> getAllMajors() {
        return majorService.getAllMajors().stream().map(majorMapper::majorToMajorDTO).collect(Collectors.toList());
    }

    @GetMapping("/{id}")
    public MajorDTO getMajorById(@PathVariable Long id){
        return majorMapper.majorToMajorDTO(majorService.getMajorById(id));
    }

    @GetMapping("/search")
    public MajorDTO searchMajor(@RequestParam(value = "name", required = true) String name) {
        return majorMapper.majorToMajorDTO(majorService.searchMajor(name));
    }

    @PutMapping("/{id}")
    public MajorDTO updateMajor(@RequestBody Long id,
                                @RequestBody String name) {
        return majorMapper.majorToMajorDTO(majorService.updateMajor(id, name));
    }

    @DeleteMapping("/{id}")
    public boolean deleteMajor(@RequestParam Long id) {
        return majorService.deleteMajor(id);
    }
}
