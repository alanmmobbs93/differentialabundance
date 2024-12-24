
import nextflow.Nextflow
import org.yaml.snakeyaml.Yaml

class YMLProcessing {

    /**
     * Parses a YAML file to extract contrast data.
     *
     * @param ymlFile The YAML file to parse.
     * @return A list of maps, each containing keys: id, variable, reference, and target.
     */
    public static List<Map<String, String>> parseContrastsFromYML(ymlFile) {
        //if (!file(ymlFile).exists() || !file(ymlFile).canRead()) {
        //    throw new IllegalArgumentException("Invalid file: ${ymlFile}. Ensure it exists and is readable.")
        //}

        def yaml = new Yaml()
        def yamlData = yaml.load(ymlFile.text)

        if (!yamlData?.contrasts) {
            throw new IllegalArgumentException("Invalid YAML structure: Missing 'contrasts' key.")
        }

        def tuples = yamlData.contrasts.collect { contrast ->
            if (!contrast?.id || !contrast?.comparison || contrast.comparison.size() < 3) {
                throw new IllegalArgumentException("Invalid contrast data: ${contrast}")
            }

            [
                id: contrast.id,
                contrast_variable: contrast.comparison[0],
                reference: contrast.comparison[1],
                target: contrast.comparison[2],
                blocking_factors: blocking_factors
            ]
        }

        return tuples
    }

}

