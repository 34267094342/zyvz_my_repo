#!/usr/bin/env python3
"""
Branch Duplicate Analyzer
Analyzes repository branches to identify duplicates that perform the same function.

Turkish: "repositorimde fazladan bir branch varsa aynı işi yapaın sil"
English: "If there is an extra branch in my repository that does the same job, delete it"
"""

import re
from typing import List, Dict, Set, Tuple
from dataclasses import dataclass
from difflib import SequenceMatcher

@dataclass
class BranchInfo:
    """Information about a repository branch"""
    name: str
    sha: str
    protected: bool
    description: str = ""
    purpose: str = ""
    keywords: Set[str] = None
    
    def __post_init__(self):
        if self.keywords is None:
            self.keywords = set()

class BranchAnalyzer:
    """Analyzes branches to find duplicates based on functionality"""
    
    def __init__(self):
        self.branches: List[BranchInfo] = []
        self.keywords_map = {
            'abap_development': ['abap', 'guidelines', 'standards', 'development', 'clean', 'modern'],
            'documentation': ['doc', 'readme', 'documentation', 'guide', 'reference'],
            'excel_functionality': ['excel', 'download', 'dynamic', 'ole2', 'export'],
            'cloud_development': ['cloud', 'sap', 'api', 'released'],
            'feature_implementation': ['feature', 'feat', 'implementation', 'new']
        }
    
    def add_branch(self, name: str, sha: str, protected: bool, description: str = ""):
        """Add a branch for analysis"""
        branch = BranchInfo(name, sha, protected, description)
        branch.purpose = self._extract_purpose(name, description)
        branch.keywords = self._extract_keywords(name, description)
        self.branches.append(branch)
    
    def _extract_purpose(self, name: str, description: str) -> str:
        """Extract the main purpose from branch name and description"""
        text = f"{name} {description}".lower()
        
        # Map keywords to purposes
        if any(keyword in text for keyword in self.keywords_map['abap_development']):
            if 'cloud' in text:
                return 'abap_cloud_development'
            elif 'guideline' in text or 'dsag' in text:
                return 'abap_guidelines'
            else:
                return 'abap_development'
        elif any(keyword in text for keyword in self.keywords_map['excel_functionality']):
            return 'excel_functionality'
        elif any(keyword in text for keyword in self.keywords_map['documentation']):
            return 'documentation'
        elif 'fix' in name and 'copilot' in name:
            return 'copilot_fix'
        elif 'feat' in name:
            return 'feature_branch'
        else:
            return 'unknown'
    
    def _extract_keywords(self, name: str, description: str) -> Set[str]:
        """Extract keywords from branch name and description"""
        text = f"{name} {description}".lower()
        keywords = set()
        
        # Extract meaningful words
        words = re.findall(r'\b\w+\b', text)
        for word in words:
            if len(word) > 2 and word not in ['the', 'and', 'for', 'with', 'this', 'that']:
                keywords.add(word)
        
        return keywords
    
    def find_duplicates(self) -> List[Tuple[BranchInfo, BranchInfo, float]]:
        """Find branches that appear to be duplicates"""
        duplicates = []
        
        for i, branch1 in enumerate(self.branches):
            for branch2 in self.branches[i+1:]:
                similarity = self._calculate_similarity(branch1, branch2)
                # Lower threshold for ABAP development related branches
                threshold = 0.3 if self._is_abap_related(branch1) and self._is_abap_related(branch2) else 0.6
                if similarity > threshold:
                    duplicates.append((branch1, branch2, similarity))
        
        return sorted(duplicates, key=lambda x: x[2], reverse=True)
    
    def _is_abap_related(self, branch: BranchInfo) -> bool:
        """Check if branch is ABAP development related"""
        abap_purposes = ['abap_development', 'abap_cloud_development', 'abap_guidelines']
        return branch.purpose in abap_purposes
    
    def _calculate_similarity(self, branch1: BranchInfo, branch2: BranchInfo) -> float:
        """Calculate similarity between two branches"""
        # Same purpose adds significant similarity
        purpose_similarity = 1.0 if branch1.purpose == branch2.purpose else 0.0
        
        # Special case: different ABAP purposes but still overlapping
        abap_purposes = ['abap_development', 'abap_cloud_development', 'abap_guidelines']
        if branch1.purpose in abap_purposes and branch2.purpose in abap_purposes:
            purpose_similarity = max(purpose_similarity, 0.7)  # High similarity for ABAP branches
        
        # Keyword overlap
        common_keywords = branch1.keywords.intersection(branch2.keywords)
        total_keywords = branch1.keywords.union(branch2.keywords)
        keyword_similarity = len(common_keywords) / len(total_keywords) if total_keywords else 0.0
        
        # Description similarity
        desc_similarity = SequenceMatcher(None, branch1.description, branch2.description).ratio()
        
        # Both are copilot fix branches
        if 'copilot/fix' in branch1.name and 'copilot/fix' in branch2.name:
            base_similarity = 0.4
        else:
            base_similarity = 0.0
        
        # Weighted average
        return max(base_similarity, purpose_similarity * 0.4 + keyword_similarity * 0.3 + desc_similarity * 0.3)
    
    def generate_cleanup_recommendations(self) -> Dict[str, List[str]]:
        """Generate recommendations for branch cleanup"""
        duplicates = self.find_duplicates()
        recommendations = {
            'delete': [],
            'keep': [],
            'merge_candidates': []
        }
        
        processed_branches = set()
        
        for branch1, branch2, similarity in duplicates:
            if branch1.name in processed_branches or branch2.name in processed_branches:
                continue
            
            # Protect main and protected branches
            if branch1.protected or 'main' in branch1.name.lower():
                recommendations['keep'].append(branch1.name)
                recommendations['delete'].append(branch2.name)
            elif branch2.protected or 'main' in branch2.name.lower():
                recommendations['keep'].append(branch2.name)
                recommendations['delete'].append(branch1.name)
            else:
                # Keep the one with more descriptive name or more recent
                if len(branch1.name) > len(branch2.name) or 'fix' in branch1.name:
                    recommendations['keep'].append(branch1.name)
                    recommendations['delete'].append(branch2.name)
                else:
                    recommendations['keep'].append(branch2.name)
                    recommendations['delete'].append(branch1.name)
            
            processed_branches.add(branch1.name)
            processed_branches.add(branch2.name)
        
        return recommendations
    
    def generate_report(self) -> str:
        """Generate a detailed analysis report"""
        report = ["# Branch Analysis Report", ""]
        report.append("## Repository Branch Analysis")
        report.append(f"Total branches analyzed: {len(self.branches)}")
        report.append("")
        
        # Group by purpose
        purposes = {}
        for branch in self.branches:
            if branch.purpose not in purposes:
                purposes[branch.purpose] = []
            purposes[branch.purpose].append(branch)
        
        report.append("## Branches by Purpose:")
        for purpose, branches in purposes.items():
            report.append(f"### {purpose.replace('_', ' ').title()}")
            for branch in branches:
                report.append(f"- `{branch.name}` (protected: {branch.protected})")
        report.append("")
        
        # Find duplicates
        duplicates = self.find_duplicates()
        if duplicates:
            report.append("## Potential Duplicates Found:")
            for branch1, branch2, similarity in duplicates:
                report.append(f"### Similarity: {similarity:.2%}")
                report.append(f"- Branch 1: `{branch1.name}` (Purpose: {branch1.purpose})")
                report.append(f"- Branch 2: `{branch2.name}` (Purpose: {branch2.purpose})")
                report.append(f"- Common keywords: {', '.join(branch1.keywords.intersection(branch2.keywords))}")
                report.append("")
        
        # Recommendations
        recommendations = self.generate_cleanup_recommendations()
        if recommendations['delete']:
            report.append("## Cleanup Recommendations:")
            report.append("### Branches to Delete (Duplicates):")
            for branch in recommendations['delete']:
                report.append(f"- `{branch}`")
            report.append("")
            report.append("### Branches to Keep:")
            for branch in recommendations['keep']:
                report.append(f"- `{branch}`")
        else:
            report.append("## No duplicate branches found that need cleanup.")
        
        return "\n".join(report)

def analyze_current_repository():
    """Analyze the current repository's branches"""
    # This function would be enhanced with actual GitHub API calls
    # For now, we'll use the known branch information from our analysis
    
    analyzer = BranchAnalyzer()
    
    # Add known branches from GitHub API response
    known_branches = [
        ("copilot/fix-7fad97d0-9f9e-4084-b501-4484d67ca13e", "403f98ab", False, 
         "ABAP Cloud Development content implementation with comprehensive documentation and examples"),
        ("copilot/fix-8170984b-a57a-4cb4-a784-3eda1bb4fb13", "7677693c", False,
         "DSAG ABAP Guidelines integration and modern ABAP development practices"),
        ("copilot/fix-8705089f-114e-44e4-a04d-67027daf02e5", "e648dc8f", False,
         "Dynamic Excel ABAP code collection for Excel download functionality"),
        ("copilot/fix-c05872b1-5b71-47ae-a788-70d5a11bd1b5", "efb1a901", False,
         "Branch cleanup to remove duplicates that do the same job"),
        ("feat/abap-zitetr-fat2eirsal-olustur", "ad98dc15", False,
         "Feature implementation for ABAP report zitetr_fat2eirsal_olustur"),
        ("main", "f8cd4ef7", True, "Main branch")
    ]
    
    for name, sha, protected, description in known_branches:
        analyzer.add_branch(name, sha, protected, description)
    
    return analyzer

if __name__ == "__main__":
    analyzer = analyze_current_repository()
    print(analyzer.generate_report())