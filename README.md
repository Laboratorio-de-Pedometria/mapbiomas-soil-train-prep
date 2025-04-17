# Processing Training Data for MapBiomas Soil Organic Carbon and Texture Maps in Brazil

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Production](https://img.shields.io/badge/status-production-green.svg)](https://shields.io/)
[![MapBiomas Soil Collection 2](https://img.shields.io/badge/MapBiomas%20Soil-Collection%202-blue.svg)](https://shields.io/)
[![DOI SOC Data](https://img.shields.io/badge/DOI-10.60502%2FSoidData%2FSXCSDK-blue)](https://doi.org/10.60502/SoilData/SXCSDK)
[![DOI Texture Data](https://img.shields.io/badge/DOI-10.60502%2FSoidData%2FP6R332-blue)](https://doi.org/10.60502/SoilData/P6R332)

## Introduction

Mapping soil properties is crucial for understanding and managing key environmental processes. Soil organic carbon (SOC) stock is vital for the carbon cycle and climate regulation, while soil particle size distribution (clay, silt, and sand content) influences water retention, nutrient availability, and soil structure. Both SOC and texture play significant roles in maintaining soil health, enhancing agricultural productivity, and supporting biodiversity. Monitoring these properties allows researchers and policymakers to assess the impacts of land-use changes, agricultural practices, and conservation efforts.

This repository is part of an ongoing effort by the MapBiomas Soil to monitor and analyze changes in soil properties across Brazil. It contains the source code used to standardize and harmonize field soil data from various sources, especially the SoilData repository (https://soildata.mapbiomas.org/). This harmonized data, covering both soil organic carbon and particle size distribution, is then used to train machine learning algorithms alongside environmental covariates. These models produce:
*   A series of **annual maps of soil organic carbon stock** in Brazil from 1985 to the present.
*   Maps of **soil texture fractions (clay, silt, sand)** for three depth layers (0-10 cm, 10-20 cm, and 20-30 cm) with a nominal reference year of 1990.

## Installation and usage

### Repository Structure

This repository follows a standard project structure to organize code, data, and results:

*   **`/data`**: Contains the raw input data files (e.g., CSV, TSV, TXT) required to run the processing scripts. *Note: This directory is typically excluded from version control (via `.gitignore`) as input data can be large or sourced externally.*
*   **`/doc`**: Includes documentation files, primarily in Markdown (`.md`), related to the project or data processing steps. *Note: Some documentation here might be outdated; the primary, up-to-date workflow logic resides in the `/src` directory.*
*   **`/res`**: Stores the results generated by executing the scripts and notebooks in `/src`.
    *   **`/res/fig`**: Contains generated figures, plots, and maps (e.g., PNG files).
    *   **`/res/tab`**: Holds tabular results, including intermediate tables and the final harmonized training dataset (e.g., CSV, TSV, TXT files).
    *   *Note: The `/res` directory and its subdirectories are typically excluded from version control (via `.gitignore`) as they contain generated artifacts derived from the source code and input data.*
*   **`/src`**: Holds all the source code for the data processing and analysis workflow. This includes R scripts (`.r`) and Jupyter Notebooks (`.ipynb`), organized sequentially for execution. This is the core directory for understanding and running the project.
*   **`/tmp`**: Used for storing temporary files generated during script execution, which are not essential outputs. *Note: This directory is typically excluded from version control (via `.gitignore`).*

### Requirements

To execute the code in this repository, you will need both R and Python installed on your system, along with several specific packages and libraries.

#### Environment Setup

*   **R:** It's recommended to use the latest stable version of R. You can install packages directly from the R console. The scripts include conditional installation checks (`if (!require(...)) install.packages(...)`), but ensuring they are installed beforehand is good practice. Consider using a project management tool like `renv` for better reproducibility if you plan extensive modifications.
*   **Python:** It's highly recommended to use a virtual environment (e.g., using `venv` or `conda`) to manage Python dependencies and avoid conflicts with other projects.

#### R Packages

The following R packages are required. You can install them using the `install.packages()` command in R (except for `febr`, which requires `remotes`).

*   `data.table`: Fast data manipulation (reading, writing, joining, aggregating).
    ```R
    # install.packages("data.table")
    ```
*   `sf`: Handling and processing simple features (spatial vector data).
    ```R
    # install.packages("sf")
    ```
*   `geobr`: Access to official spatial datasets of Brazil.
    ```R
    # install.packages("geobr")
    ```
*   `rnaturalearth`: Access to Natural Earth map data.
    ```R
    # install.packages("rnaturalearth")
    ```
*   `prettymapr`: Tools for creating nice map elements (scale bars, north arrows). *Note: May be installed as a dependency or used implicitly.*
    ```R
    # install.packages("prettymapr")
    ```
*   `ranger`: Fast implementation of Random Forests for modeling.
    ```R
    # install.packages("ranger")
    ```
*   `febr`: Access to the Brazilian Soil Data Repository (requires `remotes` package).
    ```R
    # if (!require("remotes")) install.packages("remotes")
    # remotes::install_github("laboratorio-de-pedometria/febr-R")
    ```
*   `qmap`: Functions for quantile mapping, often used in bias correction.
    ```R
    # install.packages("qmap")
    ```

#### Python Libraries

The following Python libraries are required. You can install them using `pip` within your activated virtual environment. Consider creating a `requirements.txt` file for easier installation (`pip install -r requirements.txt`).

*   `ee`: Google Earth Engine Python API client.
    ```bash
    # pip install earthengine-api
    ```
    *Note: Requires authentication after installation (`earthengine authenticate`).*
*   `geemap`: Interactive mapping with Google Earth Engine, IPyleaflet, and Folium.
    ```bash
    # pip install geemap
    ```
*   `folium`: Data visualization library for interactive maps. Often installed as a dependency of `geemap`.
    ```bash
    # pip install folium
    ```
*   `os`: Built-in library for interacting with the operating system (no installation needed).
*   `pandas`: Powerful library for data manipulation and analysis.
    ```bash
    # pip install pandas
    ```
*   `datetime`: Built-in library for working with dates and times (no installation needed).

Please ensure all these dependencies are installed before attempting to run the workflow scripts.

### Usage

The data processing workflow is implemented through a series of R scripts (`.r`) and Jupyter Notebooks (`.ipynb`), all located within the `src` directory. These files are designed to be executed sequentially to process the input data and generate the final harmonized training dataset.

To run the workflow:

1.  **Prerequisites:** Ensure you have installed R, Python, and all the required packages and libraries listed in the Requirements section.
2.  **Execution Order:** The scripts and notebooks are numbered (e.g., `01_...`, `02_...`) to indicate the correct execution sequence. It is crucial to run them in this numerical order, as each step often depends on the output of the previous one.
3.  **Running the Code:**
    *   Execute the R scripts (files ending in `.r`) using your preferred R environment (like RStudio or the R console).
    *   Execute the Jupyter Notebooks (files ending in `.ipynb`) using a Jupyter environment (like Jupyter Lab or Jupyter Notebook).

Follow the numbered sequence within the `src` directory to ensure the data is processed correctly from initial input to final output.

### Output Data

The primary outputs of this data processing workflow are stored within the `/res` directory. **Please note that, as mentioned in the Repository Structure section, the `/res` directory and its contents are typically excluded from version control (via `.gitignore`) and are therefore not synced with the repository.** You will need to run the workflow locally to generate these files.

The main output locations are:

*   **`/res/tab`**: This folder contains the main tabular data outputs. This includes:
    *   Intermediate datasets generated during the processing steps.
    *   The final, harmonized training datasets for both Soil Organic Carbon (SOC) and Soil Texture (particle size distribution - clay, silt, sand). These are the key datasets intended for use in subsequent machine learning model training.
    *   File names typically include the date of creation (e.g., `YYYY-MM-DD_dataset_name.csv`), allowing users to easily identify the latest versions generated by a workflow run.
*   **`/res/fig`**: This folder contains any figures, plots, or maps generated during the analysis or quality control steps.

**Note:** While this repository generates the harmonized training datasets, the final versions used for producing the official MapBiomas Soil maps (both the annual SOC maps and the 1990 texture maps) are typically curated and made publicly available via the SoilData repository (<https://soildata.mapbiomas.org/>). The data generated here represents the input prepared for that final stage.

## License

The source code in this repository is licensed under the MIT License - see the `LICENSE` file for details. In simple terms, this means you are generally free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the software, as long as you include the original copyright notice and license text.

If you use the code or the data generated by this workflow in your work, please:

1.  **Cite the relevant dataset(s):**
    *   For Soil Organic Carbon data: [!DOI: SOC Data](https://doi.org/10.60502/SoilData/SXCSDK)
    *   For Soil Texture data: [!DOI: Texture Data](https://doi.org/10.60502/SoilData/P6R332)
2.  **Acknowledge the MapBiomas Soil project.**

Please also ensure you comply with the terms of use for any original input data sources utilized by this workflow.

## Support and Contributions

*   **General Inquiries:** For questions about the overall MapBiomas Soil project, please use the main contact form: https://brasil.mapbiomas.org/contato/
*   **Code-Specific Issues:** For questions, bug reports, or suggestions specifically related to the code in *this* repository, please open an issue on the [GitHub Issues](https://github.com/Laboratorio-de-Pedometria/mapbiomas-soc/issues) tab. While this project is primarily maintained by the MapBiomas Soil team, feedback via Issues is welcome.
