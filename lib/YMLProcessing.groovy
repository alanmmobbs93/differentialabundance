
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

        def tuples = yamlData.contrasts.collect { contrasts ->
            if (!contrasts?.id || !contrasts?.comparison || contrasts.comparison.size() < 3) {
                throw new IllegalArgumentException("Invalid contrast data: ${contrasts}")
            }

            [
                id: contrasts.id,
                contrast_variable: contrasts.comparison[0],
                reference: contrasts.comparison[1],
                target: contrasts.comparison[2],
                blocking_factors: contrasts.blocking_factors ?: null,              // Handle missing blocking_factors
                exclude_samples_col: contrasts.exclude_samples_col ?: null,        // Handle missing exclude_samples_col
                exclude_samples_values: contrasts.exclude_samples_values ?: null,  // Handle missing exclude_samples_col
            ]
        }

        return tuples
    }

}

