-- =====================================================
-- CROP YIELD DATA CLEANING SCRIPT FOR MYSQL
-- Professional Data Cleaning Approach
-- =====================================================

-- Step 1: Create the main table structure
DROP TABLE IF EXISTS crop_yield_raw;
CREATE TABLE crop_yield_raw (
    id INT AUTO_INCREMENT PRIMARY KEY,
    crop VARCHAR(100),
    crop_year INT,
    season VARCHAR(50),
    state VARCHAR(100),
    area DECIMAL(15,2),
    production BIGINT,
    annual_rainfall DECIMAL(10,2),
    fertilizer DECIMAL(15,2),
    pesticide DECIMAL(15,2),
    yield DECIMAL(15,6),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Step 2: Create cleaned table structure
DROP TABLE IF EXISTS crop_yield_cleaned;
CREATE TABLE crop_yield_cleaned (
    id INT AUTO_INCREMENT PRIMARY KEY,
    crop VARCHAR(100),
    crop_year INT,
    season VARCHAR(50),
    state VARCHAR(100),
    area DECIMAL(15,2),
    production BIGINT,
    annual_rainfall DECIMAL(10,2),
    fertilizer DECIMAL(15,2),
    pesticide DECIMAL(15,2),
    yield DECIMAL(15,6),
    data_quality_flag ENUM('clean', 'outlier', 'zero_yield', 'extreme_value') DEFAULT 'clean',
    cleaning_notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- =====================================================
-- DATA QUALITY ANALYSIS QUERIES
-- =====================================================

-- Query 1: Basic data quality overview
SELECT 
    'Data Quality Overview' as analysis_type,
    COUNT(*) as total_records,
    COUNT(DISTINCT crop) as unique_crops,
    COUNT(DISTINCT state) as unique_states,
    COUNT(DISTINCT season) as unique_seasons,
    MIN(crop_year) as earliest_year,
    MAX(crop_year) as latest_year
FROM crop_yield_raw;

-- Query 2: Identify season name issues (trailing spaces)
SELECT 
    'Season Name Issues' as analysis_type,
    season,
    LENGTH(season) as length,
    COUNT(*) as frequency
FROM crop_yield_raw 
GROUP BY season 
ORDER BY season;

-- Query 3: Yield distribution analysis
SELECT 
    'Yield Distribution' as analysis_type,
    COUNT(*) as total_records,
    MIN(yield) as min_yield,
    MAX(yield) as max_yield,
    AVG(yield) as avg_yield,
    STDDEV(yield) as std_dev_yield,
    COUNT(CASE WHEN yield = 0 THEN 1 END) as zero_yield_count,
    COUNT(CASE WHEN yield > 1000 THEN 1 END) as extreme_high_yield_count
FROM crop_yield_raw;

-- Query 4: Identify extreme outliers (using IQR method)
WITH yield_stats AS (
    SELECT 
        yield,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY yield) OVER() as q1,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY yield) OVER() as q3
    FROM crop_yield_raw 
    WHERE yield > 0
),
outlier_analysis AS (
    SELECT 
        yield,
        q1,
        q3,
        (q3 - q1) as iqr,
        (q1 - 1.5 * (q3 - q1)) as lower_bound,
        (q3 + 1.5 * (q3 - q1)) as upper_bound
    FROM yield_stats
    LIMIT 1
)
SELECT 
    'Outlier Analysis' as analysis_type,
    lower_bound,
    upper_bound,
    COUNT(CASE WHEN yield < lower_bound OR yield > upper_bound THEN 1 END) as outlier_count
FROM crop_yield_raw, outlier_analysis;

-- Query 5: Zero yield investigation
SELECT 
    'Zero Yield Analysis' as analysis_type,
    crop,
    state,
    season,
    crop_year,
    area,
    production,
    annual_rainfall,
    fertilizer,
    pesticide
FROM crop_yield_raw 
WHERE yield = 0 
ORDER BY crop_year DESC, state, crop
LIMIT 20;

-- =====================================================
-- DATA CLEANING OPERATIONS
-- =====================================================

-- Step 3: Clean and insert data into cleaned table
INSERT INTO crop_yield_cleaned (
    crop, crop_year, season, state, area, production, 
    annual_rainfall, fertilizer, pesticide, yield, 
    data_quality_flag, cleaning_notes
)
SELECT 
    TRIM(crop) as crop,
    crop_year,
    TRIM(season) as season,  -- Remove trailing spaces
    TRIM(state) as state,
    area,
    production,
    annual_rainfall,
    fertilizer,
    pesticide,
    yield,
    CASE 
        WHEN yield = 0 THEN 'zero_yield'
        WHEN yield > 1000 THEN 'extreme_value'  -- Flag unrealistic yields
        WHEN yield < 0 THEN 'outlier'  -- Negative yields (if any)
        ELSE 'clean'
    END as data_quality_flag,
    CASE 
        WHEN yield = 0 THEN 'Zero yield - investigate crop failure or data entry error'
        WHEN yield > 1000 THEN 'Extreme yield value - requires validation'
        WHEN yield < 0 THEN 'Negative yield - data error'
        ELSE 'Clean data'
    END as cleaning_notes
FROM crop_yield_raw;

-- =====================================================
-- POST-CLEANING VALIDATION QUERIES
-- =====================================================

-- Query 6: Post-cleaning data quality summary
SELECT 
    'Post-Cleaning Summary' as analysis_type,
    data_quality_flag,
    COUNT(*) as record_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM crop_yield_cleaned), 2) as percentage
FROM crop_yield_cleaned 
GROUP BY data_quality_flag
ORDER BY record_count DESC;

-- Query 7: Clean data statistics
SELECT 
    'Clean Data Statistics' as analysis_type,
    COUNT(*) as clean_records,
    COUNT(DISTINCT crop) as unique_crops,
    COUNT(DISTINCT state) as unique_states,
    MIN(yield) as min_yield,
    MAX(yield) as max_yield,
    AVG(yield) as avg_yield,
    STDDEV(yield) as std_dev_yield
FROM crop_yield_cleaned 
WHERE data_quality_flag = 'clean';

-- Query 8: Season standardization results
SELECT 
    'Season Standardization' as analysis_type,
    season,
    COUNT(*) as frequency
FROM crop_yield_cleaned 
GROUP BY season 
ORDER BY season;

-- Query 9: Top crops by average yield (clean data only)
SELECT 
    'Top Crops by Yield' as analysis_type,
    crop,
    COUNT(*) as records,
    ROUND(AVG(yield), 2) as avg_yield,
    ROUND(MIN(yield), 2) as min_yield,
    ROUND(MAX(yield), 2) as max_yield
FROM crop_yield_cleaned 
WHERE data_quality_flag = 'clean'
GROUP BY crop 
HAVING COUNT(*) >= 10  -- Only crops with sufficient data
ORDER BY avg_yield DESC 
LIMIT 15;

-- Query 10: State-wise productivity analysis (clean data only)
SELECT 
    'State Productivity' as analysis_type,
    state,
    COUNT(*) as records,
    ROUND(AVG(yield), 2) as avg_yield,
    ROUND(SUM(production), 0) as total_production
FROM crop_yield_cleaned 
WHERE data_quality_flag = 'clean'
GROUP BY state 
ORDER BY avg_yield DESC 
LIMIT 15;

-- =====================================================
-- DATA EXPORT QUERIES
-- =====================================================

-- Query 11: Export clean data for analysis
SELECT 
    crop,
    crop_year,
    season,
    state,
    area,
    production,
    annual_rainfall,
    fertilizer,
    pesticide,
    yield
FROM crop_yield_cleaned 
WHERE data_quality_flag = 'clean'
ORDER BY crop_year, state, crop;

-- Query 12: Export flagged data for manual review
SELECT 
    crop,
    crop_year,
    season,
    state,
    area,
    production,
    annual_rainfall,
    fertilizer,
    pesticide,
    yield,
    data_quality_flag,
    cleaning_notes
FROM crop_yield_cleaned 
WHERE data_quality_flag != 'clean'
ORDER BY data_quality_flag, crop_year DESC;

-- =====================================================
-- ADDITIONAL DATA QUALITY CHECKS
-- =====================================================

-- Query 13: Check for duplicate records
SELECT 
    'Duplicate Check' as analysis_type,
    crop, crop_year, season, state,
    COUNT(*) as duplicate_count
FROM crop_yield_cleaned 
GROUP BY crop, crop_year, season, state
HAVING COUNT(*) > 1
ORDER BY duplicate_count DESC;

-- Query 14: Area vs Production consistency check
SELECT 
    'Area-Production Consistency' as analysis_type,
    COUNT(*) as total_records,
    COUNT(CASE WHEN area > 0 AND production = 0 THEN 1 END) as zero_production_with_area,
    COUNT(CASE WHEN area = 0 AND production > 0 THEN 1 END) as production_without_area,
    COUNT(CASE WHEN area > 0 AND production > 0 AND yield = 0 THEN 1 END) as zero_yield_with_production
FROM crop_yield_cleaned;

-- Query 15: Year-over-year data availability
SELECT 
    'Year Coverage' as analysis_type,
    crop_year,
    COUNT(*) as records,
    COUNT(DISTINCT state) as states_covered,
    COUNT(DISTINCT crop) as crops_covered
FROM crop_yield_cleaned 
GROUP BY crop_year 
ORDER BY crop_year;
