#!/usr/bin/env python3
"""
Branch Cleanup Script
Automatically cleans up duplicate branches that perform the same function.

Turkish: "repositorimde fazladan bir branch varsa aynı işi yapaın sil"
English: "If there is an extra branch in my repository that does the same job, delete it"
"""

import subprocess
import sys
from typing import List, Dict
from branch_analyzer import analyze_current_repository

class BranchCleaner:
    """Handles the cleanup of duplicate branches"""
    
    def __init__(self, dry_run: bool = True):
        self.dry_run = dry_run
        self.deleted_branches = []
        self.errors = []
    
    def cleanup_duplicates(self) -> Dict[str, List[str]]:
        """Clean up duplicate branches based on analysis"""
        analyzer = analyze_current_repository()
        recommendations = analyzer.generate_cleanup_recommendations()
        
        print("🔍 Branch Analysis Complete")
        print(f"📊 Branches to delete: {len(recommendations['delete'])}")
        print(f"✅ Branches to keep: {len(recommendations['keep'])}")
        print()
        
        if not recommendations['delete']:
            print("✨ No duplicate branches found that need cleanup!")
            return {"deleted": [], "errors": []}
        
        print("🧹 Starting cleanup process...")
        print(f"{'DRY RUN - ' if self.dry_run else ''}Deleting duplicate branches:")
        print()
        
        for branch_name in recommendations['delete']:
            self._delete_branch(branch_name)
        
        print()
        print(f"✅ Cleanup complete!")
        print(f"🗑️  Deleted branches: {len(self.deleted_branches)}")
        if self.errors:
            print(f"❌ Errors: {len(self.errors)}")
            for error in self.errors:
                print(f"   - {error}")
        
        return {
            "deleted": self.deleted_branches,
            "errors": self.errors
        }
    
    def _delete_branch(self, branch_name: str) -> bool:
        """Delete a single branch"""
        try:
            print(f"🗑️  {'[DRY RUN] ' if self.dry_run else ''}Deleting branch: {branch_name}")
            
            if self.dry_run:
                print(f"   Would execute: git push origin --delete {branch_name}")
                self.deleted_branches.append(branch_name)
                return True
            
            # Actually delete the branch from remote
            result = subprocess.run(
                ["git", "push", "origin", "--delete", branch_name],
                capture_output=True,
                text=True,
                cwd="."
            )
            
            if result.returncode == 0:
                print(f"   ✅ Successfully deleted remote branch: {branch_name}")
                self.deleted_branches.append(branch_name)
                return True
            else:
                error_msg = f"Failed to delete {branch_name}: {result.stderr}"
                print(f"   ❌ {error_msg}")
                self.errors.append(error_msg)
                return False
                
        except Exception as e:
            error_msg = f"Error deleting {branch_name}: {str(e)}"
            print(f"   ❌ {error_msg}")
            self.errors.append(error_msg)
            return False
    
    def preview_cleanup(self) -> str:
        """Generate a preview of what would be cleaned up"""
        analyzer = analyze_current_repository()
        recommendations = analyzer.generate_cleanup_recommendations()
        
        preview = ["# Branch Cleanup Preview", ""]
        
        if recommendations['delete']:
            preview.append("## Branches that will be DELETED (duplicates):")
            for branch in recommendations['delete']:
                preview.append(f"- `{branch}` - Identified as duplicate functionality")
            preview.append("")
            
            preview.append("## Branches that will be KEPT:")
            for branch in recommendations['keep']:
                preview.append(f"- `{branch}` - Primary implementation")
            preview.append("")
            
            preview.append("## Commands that will be executed:")
            for branch in recommendations['delete']:
                preview.append(f"```bash")
                preview.append(f"git push origin --delete {branch}")
                preview.append(f"```")
            
        else:
            preview.append("No duplicate branches found that need cleanup.")
        
        return "\n".join(preview)

def main():
    """Main function to handle command line execution"""
    if len(sys.argv) > 1 and sys.argv[1] == "--execute":
        print("⚠️  REAL EXECUTION MODE - This will actually delete branches!")
        response = input("Are you sure you want to proceed? (yes/no): ")
        if response.lower() != 'yes':
            print("Operation cancelled.")
            return
        
        cleaner = BranchCleaner(dry_run=False)
        result = cleaner.cleanup_duplicates()
    else:
        print("🏃 DRY RUN MODE - No branches will actually be deleted")
        print("Use --execute flag to perform actual cleanup")
        print()
        
        cleaner = BranchCleaner(dry_run=True)
        result = cleaner.cleanup_duplicates()
        
        print()
        print("Preview of cleanup:")
        print(cleaner.preview_cleanup())

if __name__ == "__main__":
    main()