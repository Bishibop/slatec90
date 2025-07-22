#!/usr/bin/env python3
"""
Parallel SLATEC Migration Helper

This script helps simulate parallel migration workflows by launching
multiple migration tasks that can be run in separate terminals.
"""

import sys
import os
import json
import subprocess
from datetime import datetime

class ParallelMigrationManager:
    def __init__(self):
        self.migration_dir = os.path.dirname(os.path.abspath(__file__))
        self.active_migrations = self.load_active_migrations()
        
    def load_active_migrations(self):
        """Load list of currently active migrations"""
        status_file = os.path.join(self.migration_dir, ".migration_status.json")
        if os.path.exists(status_file):
            with open(status_file) as f:
                return json.load(f)
        return {"active": [], "completed": []}
    
    def save_active_migrations(self):
        """Save current migration status"""
        status_file = os.path.join(self.migration_dir, ".migration_status.json")
        with open(status_file, 'w') as f:
            json.dump(self.active_migrations, f, indent=2)
    
    def start_migration(self, function_name):
        """Start a new migration for a function"""
        if function_name in self.active_migrations["active"]:
            print(f"ERROR: {function_name} is already being migrated!")
            return False
            
        if function_name in self.active_migrations["completed"]:
            print(f"ERROR: {function_name} has already been migrated!")
            return False
        
        # Add to active migrations
        self.active_migrations["active"].append({
            "function": function_name,
            "started": datetime.now().isoformat(),
            "pid": os.getpid()
        })
        self.save_active_migrations()
        
        print(f"Starting migration for {function_name}")
        print(f"Run this in a new terminal:")
        print(f"\ncd {self.migration_dir}")
        print(f"python parallel_migration.py run {function_name}\n")
        
        return True
    
    def run_migration(self, function_name):
        """Run the actual migration for a function"""
        print(f"\n{'='*60}")
        print(f"MIGRATING: {function_name}")
        print(f"{'='*60}\n")
        
        # Create prompts for each phase
        test_gen_prompt = f"""
You are migrating the SLATEC function {function_name} from F77 to modern Fortran.

Phase 1: Test Generation
1. Read the F77 source at src/{function_name.lower()}.f
2. Understand what the function does
3. Add support to slatec_test_helper.py:
   - Add to generate_tests() method
   - Implement _generate_{function_name.lower()}_tests()
   - Implement _generate_{function_name.lower()}_f77()
4. Generate 150-200 comprehensive test cases
5. Run: python slatec_test_helper.py generate {function_name}
6. Save results to test_data/{function_name.lower()}_tests.json
7. Create blind version at test_data/{function_name.lower()}_tests_blind.json

Focus on edge cases, numerical stability, and mathematical properties.
"""

        impl_prompt = f"""
Phase 2: Blind Implementation
1. Read F77 source at src/{function_name.lower()}.f
2. Read blind tests from test_data/{function_name.lower()}_tests_blind.json
3. Create modern/={function_name.lower()}_modern.f90
4. Implement the algorithm in modern Fortran
5. DO NOT look at expected outputs
6. Generate your outputs for validation

Remember: You must understand and translate the algorithm, not fit to test outputs.
"""

        validate_prompt = f"""
Phase 3: Validation
1. Compare implementation outputs with test_data/{function_name.lower()}_tests.json
2. Report pass/fail rate
3. If failures, provide hints without revealing expected values
4. Iterate until 100% pass rate
5. Update MIGRATION_GUIDE.md to mark {function_name} as completed
"""

        # Display instructions
        print("Copy and paste these prompts to Task tool:\n")
        print("1. TEST GENERATION:")
        print("-" * 40)
        print(test_gen_prompt)
        print("\n2. BLIND IMPLEMENTATION:")
        print("-" * 40)
        print(impl_prompt)
        print("\n3. VALIDATION:")
        print("-" * 40)
        print(validate_prompt)
        
    def complete_migration(self, function_name):
        """Mark a migration as completed"""
        # Remove from active
        self.active_migrations["active"] = [
            m for m in self.active_migrations["active"] 
            if m["function"] != function_name
        ]
        
        # Add to completed
        self.active_migrations["completed"].append({
            "function": function_name,
            "completed": datetime.now().isoformat()
        })
        
        self.save_active_migrations()
        print(f"Migration of {function_name} marked as complete!")
        
    def status(self):
        """Show current migration status"""
        print("\nActive Migrations:")
        print("-" * 40)
        if not self.active_migrations["active"]:
            print("None")
        else:
            for m in self.active_migrations["active"]:
                print(f"- {m['function']} (started: {m['started']})")
        
        print("\nCompleted Migrations:")
        print("-" * 40)
        for m in self.active_migrations["completed"][-10:]:  # Last 10
            print(f"- {m['function']} (completed: {m['completed']})")
            
        print(f"\nTotal completed: {len(self.active_migrations['completed'])}")

def main():
    if len(sys.argv) < 2:
        print("""
Parallel SLATEC Migration Manager

Usage:
  python parallel_migration.py start FUNCTION1 [FUNCTION2 ...]
  python parallel_migration.py run FUNCTION
  python parallel_migration.py complete FUNCTION
  python parallel_migration.py status

Examples:
  # Start parallel migrations for DENORM and ZABS
  python parallel_migration.py start DENORM ZABS
  
  # In terminal 1:
  python parallel_migration.py run DENORM
  
  # In terminal 2:
  python parallel_migration.py run ZABS
  
  # After completion:
  python parallel_migration.py complete DENORM
  python parallel_migration.py complete ZABS
""")
        sys.exit(1)
    
    manager = ParallelMigrationManager()
    command = sys.argv[1]
    
    if command == "start":
        if len(sys.argv) < 3:
            print("ERROR: Specify at least one function to migrate")
            sys.exit(1)
        
        functions = sys.argv[2:]
        for func in functions:
            manager.start_migration(func.upper())
            
    elif command == "run":
        if len(sys.argv) < 3:
            print("ERROR: Specify function to run")
            sys.exit(1)
        
        function = sys.argv[2].upper()
        manager.run_migration(function)
        
    elif command == "complete":
        if len(sys.argv) < 3:
            print("ERROR: Specify function to mark complete")
            sys.exit(1)
            
        function = sys.argv[2].upper()
        manager.complete_migration(function)
        
    elif command == "status":
        manager.status()
        
    else:
        print(f"ERROR: Unknown command '{command}'")
        sys.exit(1)

if __name__ == "__main__":
    main()