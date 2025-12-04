import os
import subprocess
import sys
import platform
import time

def open_image(path):
    if platform.system() == "Windows":
        os.startfile(path)
    elif platform.system() == "Darwin":
        subprocess.Popen(["open", path])
    else:
        subprocess.Popen(["xdg-open", path])

def main():
    base_dir = os.getcwd()
    workshops_dir = os.path.join(base_dir, "Workshops")
    
    if not os.path.exists(workshops_dir):
        print("Workshops directory not found.")
        return

    # List directories in Workshops
    weeks = sorted([d for d in os.listdir(workshops_dir) if os.path.isdir(os.path.join(workshops_dir, d))])
    
    if not weeks:
        print("No workshop folders found.")
        return

    print("\nAvailable Workshops:")
    for idx, week in enumerate(weeks):
        print(f"{idx + 1}. {week}")

    try:
        selection = input("\nSelect a workshop number to run: ")
        idx = int(selection) - 1
        
        if 0 <= idx < len(weeks):
            selected_week = weeks[idx]
            week_path = os.path.join(workshops_dir, selected_week)
            
            # 1. Look for a local lab script (excluding viz_demo.R if it were there)
            # We want to find "Introduction to R..." or similar
            r_files = [f for f in os.listdir(week_path) if f.lower().endswith('.r') and f != "viz_demo.R"]
            
            if r_files:
                lab_script = r_files[0]
                print(f"\nFound lab script: {lab_script}")
                run_lab = input("Do you want to run this lab script? (y/n): ").strip().lower()
                
                if run_lab == 'y':
                    print(f"Running {lab_script}...")
                    subprocess.run(
                        ["R", "--quiet", "--no-save", "--no-restore", "-e", f"source('{lab_script}')"],
                        cwd=week_path
                    )
            else:
                print(f"\nNo specific lab script found for {selected_week}.")

            # 2. Ask about visualizations (using the common script)
            ask_viz = input("\nDo you want to generate visualizations? (y/n): ").strip().lower()
            
            if ask_viz == 'y':
                common_viz_script = os.path.join(base_dir, "viz_demo.R")
                if os.path.exists(common_viz_script):
                    print(f"Running common visualization script...")
                    # Run the common R script from the week directory
                    result = subprocess.run(
                        ["R", "--quiet", "--no-save", "--no-restore", "-e", f"source('{common_viz_script}')"],
                        cwd=week_path,
                        capture_output=True,
                        text=True
                    )
                    
                    print(result.stdout)
                    if result.stderr:
                        errors = [line for line in result.stderr.splitlines() if not line.startswith("*")]
                        if errors:
                            print("Errors/Warnings:")
                            print("\n".join(errors))
                        
                    if result.returncode == 0:
                        print("\nSuccess! Opening generated images...")
                        pngs = [f for f in os.listdir(week_path) if f.lower().endswith('.png')]
                        if pngs:
                            for png in pngs:
                                print(f" - Opening {png}")
                                open_image(os.path.join(week_path, png))
                                time.sleep(0.5)
                        else:
                            print("No PNG images found in the folder.")
                    else:
                        print("\nAn error occurred during execution.")
                else:
                    print("Error: Common viz_demo.R not found in the root directory.")
            else:
                print("Skipping visualization generation.")
                
        else:
            print("Invalid selection number.")
            
    except ValueError:
        print("Please enter a valid number.")
    except KeyboardInterrupt:
        print("\nOperation cancelled.")

if __name__ == "__main__":
    main()
