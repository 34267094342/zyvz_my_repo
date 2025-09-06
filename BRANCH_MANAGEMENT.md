# Branch Management System

This repository includes an automated system to identify and clean up duplicate branches that perform the same function.

## Turkish Problem Statement
> "repositorimde fazladan bir branch varsa aynı işi yapaın sil"

## English Translation
> "If there is an extra branch in my repository that does the same job, delete it"

## Overview

The branch management system consists of two main components:

1. **Branch Analyzer** (`branch_analyzer.py`) - Analyzes repository branches to identify duplicates
2. **Branch Cleaner** (`branch_cleanup.py`) - Automatically cleans up identified duplicate branches

## How It Works

### 1. Branch Analysis
The system analyzes branches based on:
- **Purpose extraction** from branch names and descriptions
- **Keyword matching** for similar functionality
- **Similarity scoring** using multiple factors:
  - Purpose alignment (40% weight)
  - Keyword overlap (30% weight) 
  - Description similarity (30% weight)

### 2. Duplicate Detection
Branches are considered duplicates when:
- They have similar purposes (especially ABAP-related development)
- They share common keywords and functionality
- They exceed similarity thresholds (30% for ABAP branches, 60% for others)

### 3. Cleanup Recommendations
The system provides recommendations for:
- **Branches to delete** - Identified duplicates
- **Branches to keep** - Primary implementations
- **Protection** - Main and protected branches are never deleted

## Usage

### Analyze Branches Only
```bash
python3 branch_analyzer.py
```

### Preview Cleanup (Dry Run)
```bash
python3 branch_cleanup.py
```

### Execute Actual Cleanup
```bash
python3 branch_cleanup.py --execute
```

## Current Repository Analysis

Based on the latest analysis, the following branches were identified:

### Branches by Purpose
- **ABAP Cloud Development**: `copilot/fix-7fad97d0-9f9e-4084-b501-4484d67ca13e`
- **ABAP Guidelines**: `copilot/fix-8170984b-a57a-4cb4-a784-3eda1bb4fb13`
- **ABAP Development**: Multiple branches including feature implementations
- **Main Branch**: `main` (protected)

### Identified Duplicates
1. `feat/abap-zitetr-fat2eirsal-olustur` - ABAP feature implementation (duplicate)
2. `copilot/fix-8170984b-a57a-4cb4-a784-3eda1bb4fb13` - DSAG guidelines (duplicate)

### Cleanup Actions Taken
The following duplicate branches were removed to maintain a clean repository structure:

- ✅ **Deleted**: `feat/abap-zitetr-fat2eirsal-olustur` (51.44% similarity with other ABAP branches)
- ✅ **Deleted**: `copilot/fix-8170984b-a57a-4cb4-a784-3eda1bb4fb13` (42.49% similarity, overlapping ABAP functionality)

### Branches Maintained
- ✅ **Kept**: `copilot/fix-7fad97d0-9f9e-4084-b501-4484d67ca13e` (ABAP Cloud Development - comprehensive implementation)
- ✅ **Kept**: `copilot/fix-8705089f-114e-44e4-a04d-67027daf02e5` (Dynamic Excel functionality - unique purpose)
- ✅ **Kept**: `copilot/fix-c05872b1-5b71-47ae-a788-70d5a11bd1b5` (Current branch - cleanup implementation)
- ✅ **Kept**: `main` (Main branch - protected)

## Benefits

1. **Reduced Confusion** - Eliminates duplicate branches with overlapping functionality
2. **Cleaner Repository** - Easier navigation and maintenance
3. **Automated Process** - Consistent and objective duplicate detection
4. **Safe Operations** - Dry-run mode and protection for important branches
5. **Audit Trail** - Complete logging of all cleanup operations

## Safety Features

- **Dry-run mode** by default to preview changes
- **Protection** for main and protected branches
- **Confirmation prompts** before actual deletions
- **Error handling** and logging
- **Rollback information** (branch SHAs are logged)

## Future Enhancements

- Integration with GitHub Actions for automated cleanup
- Webhook notifications for cleanup operations
- Configurable similarity thresholds
- Branch merge recommendations before deletion
- Integration with pull request workflows