#!/usr/bin/env python3
"""
Create a visual summary of SLATEC complexity analysis.
"""

import json
import matplotlib.pyplot as plt
import numpy as np

# Read the analysis results
with open('slatec_complexity_analysis.json', 'r') as f:
    data = json.load(f)

# Extract data for visualization
levels = []
counts = []
percentages = []
names = []

for level in sorted(data['complexity_levels'].keys()):
    level_data = data['complexity_levels'][level]
    levels.append(f"Level {level}")
    counts.append(level_data['count'])
    percentages.append(level_data['percentage'])
    names.append(level_data['name'])

# Create figure with subplots
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Bar chart
colors = ['#2ecc71', '#3498db', '#f39c12', '#e74c3c', '#9b59b6']
bars = ax1.bar(levels, counts, color=colors)
ax1.set_ylabel('Number of Functions')
ax1.set_title('SLATEC Functions by Complexity Level')

# Add count labels on bars
for i, (bar, count) in enumerate(zip(bars, counts)):
    height = bar.get_height()
    ax1.text(bar.get_x() + bar.get_width()/2., height,
             f'{count}', ha='center', va='bottom')
    ax1.text(bar.get_x() + bar.get_width()/2., height/2,
             names[i], ha='center', va='center', fontsize=10, fontweight='bold')

# Pie chart
explode = (0.1, 0.05, 0, 0, 0.1)  # explode trivial and stateful
ax2.pie(counts, labels=[f"{names[i]}\n{counts[i]} ({percentages[i]:.1f}%)" 
                        for i in range(len(counts))], 
        colors=colors, autopct='', explode=explode, startangle=90)
ax2.set_title('Distribution of SLATEC Functions by Complexity')

plt.tight_layout()
plt.savefig('slatec_complexity_distribution.png', dpi=300, bbox_inches='tight')
print("Visualization saved as slatec_complexity_distribution.png")

# Create a feature comparison chart
fig2, ax = plt.subplots(figsize=(10, 6))

features = ['EXTERNAL', 'COMMON', 'SAVE', 'Arrays', 'Work Arrays', 'INDEX Pattern']
feature_counts = [194, 110, 164, 627, 98, 4]
feature_percentages = [c/736*100 for c in feature_counts]

y_pos = np.arange(len(features))
bars = ax.barh(y_pos, feature_percentages, color='#3498db')

ax.set_yticks(y_pos)
ax.set_yticklabels(features)
ax.set_xlabel('Percentage of Functions')
ax.set_title('Feature Usage in SLATEC Functions')

# Add percentage labels
for i, (bar, pct, count) in enumerate(zip(bars, feature_percentages, feature_counts)):
    ax.text(bar.get_width() + 0.5, bar.get_y() + bar.get_height()/2,
            f'{count} ({pct:.1f}%)', va='center')

plt.tight_layout()
plt.savefig('slatec_feature_usage.png', dpi=300, bbox_inches='tight')
print("Feature usage visualization saved as slatec_feature_usage.png")