#!/usr/bin/env python3
"""
SLATEC Migration Test Helper

This script helps generate test cases, run F77 reference implementations,
and validate modern Fortran implementations for SLATEC functions.

Usage:
    # Generate test cases and get reference values from F77
    python slatec_test_helper.py generate FUNCNAME
    
    # Validate modern implementation against test data
    python slatec_test_helper.py validate FUNCNAME
"""

import json
import subprocess
import re
import sys
import math
from pathlib import Path


class SlatecTestHelper:
    def __init__(self, func_name):
        self.func_name = func_name.upper()
        self.test_file = f"test_data/{func_name.lower()}_tests.json"
        self.batch_size = 50  # F77 program size limit
        
    def generate_test_cases(self):
        """Generate test cases based on function type"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_tests()
        elif self.func_name == "CDIV":
            return self._generate_cdiv_tests()
        elif self.func_name == "I1MACH":
            return self._generate_i1mach_tests()
        elif self.func_name == "R1MACH":
            return self._generate_r1mach_tests()
        elif self.func_name == "D1MACH":
            return self._generate_d1mach_tests()
        elif self.func_name == "ENORM":
            return self._generate_enorm_tests()
        elif self.func_name == "LSAME":
            return self._generate_lsame_tests()
        else:
            print(f"No test generator for {self.func_name} yet")
            print("Please implement a generator based on the function's purpose")
            return []
    
    def _generate_pythag_tests(self):
        """Generate test cases for PYTHAG (Pythagorean sum)"""
        tests = []
        
        # Pythagorean triples
        triples = [(3,4,5), (5,12,13), (8,15,17), (7,24,25)]
        for a, b, c in triples:
            for scale in [0.01, 0.1, 1, 10, 100, 1000]:
                tests.append({
                    "description": f"Pythagorean triple ({a},{b},{c}) scaled by {scale}",
                    "inputs": [a*scale, b*scale],
                    "expected": None
                })
        
        # Edge cases
        edge_values = [0, 1, -1, 1e-10, 1e10, 1e-38, 1e38]
        for a in edge_values[:4]:
            for b in edge_values[:4]:
                tests.append({
                    "description": f"Edge case: a={a}, b={b}",
                    "inputs": [a, b],
                    "expected": None
                })
        
        # Special patterns
        for i in range(-10, 11):
            val = 2**i
            tests.append({
                "description": f"Powers of 2: both {val}",
                "inputs": [val, val],
                "expected": None
            })
        
        return tests
    
    def _generate_cdiv_tests(self):
        """Generate test cases for CDIV (complex division)"""
        tests = []
        
        # Basic cases
        basic = [
            ("Real / real", [1.0, 0.0, 1.0, 0.0]),
            ("i / i = 1", [0.0, 1.0, 0.0, 1.0]),
            ("Complex / real", [1.0, 1.0, 2.0, 0.0]),
            ("Real / imaginary", [1.0, 0.0, 0.0, 1.0]),
            ("Complex / complex", [3.0, 4.0, 1.0, 2.0]),
        ]
        
        for desc, inputs in basic:
            tests.append({
                "description": desc,
                "inputs": inputs,
                "expected": None
            })
        
        # Angles
        for angle in range(0, 360, 30):
            rad = math.radians(angle)
            ar, ai = 5 * math.cos(rad), 5 * math.sin(rad)
            br, bi = math.cos(math.radians(45)), math.sin(math.radians(45))
            tests.append({
                "description": f"Magnitude 5 at {angle}° / unit at 45°",
                "inputs": [ar, ai, br, bi],
                "expected": None
            })
        
        return tests
    
    def _generate_i1mach_tests(self):
        """Generate test cases for I1MACH (machine constants)"""
        tests = []
        
        # Test all 16 valid indices
        descriptions = [
            "Standard input unit",
            "Standard output unit", 
            "Standard punch unit",
            "Standard error unit",
            "Bits per integer",
            "Characters per integer",
            "Integer base",
            "Integer digits",
            "Largest integer",
            "Float base",
            "Single precision digits",
            "Single precision min exponent",
            "Single precision max exponent",
            "Double precision digits",
            "Double precision min exponent",
            "Double precision max exponent"
        ]
        
        for i in range(1, 17):
            tests.append({
                "description": f"I1MACH({i}): {descriptions[i-1]}",
                "inputs": [i],
                "expected": None
            })
        
        # Test invalid indices (should cause error in F77)
        # We'll skip these for now since F77 will STOP on error
        
        return tests
    
    def _generate_r1mach_tests(self):
        """Generate test cases for R1MACH (real machine constants)"""
        tests = []
        
        # Test all 5 valid indices
        descriptions = [
            "Smallest positive magnitude",
            "Largest magnitude",
            "Smallest relative spacing (epsilon)",
            "Largest relative spacing",
            "LOG10(base)"
        ]
        
        for i in range(1, 6):
            tests.append({
                "description": f"R1MACH({i}): {descriptions[i-1]}",
                "inputs": [i],
                "expected": None
            })
        
        return tests
    
    def _generate_d1mach_tests(self):
        """Generate test cases for D1MACH (double precision machine constants)"""
        tests = []
        
        # Test all 5 valid indices
        descriptions = [
            "Smallest positive magnitude",
            "Largest magnitude",
            "Smallest relative spacing (epsilon)",
            "Largest relative spacing",
            "LOG10(base)"
        ]
        
        for i in range(1, 6):
            tests.append({
                "description": f"D1MACH({i}): {descriptions[i-1]}",
                "inputs": [i],
                "expected": None
            })
        
        return tests
    
    def _generate_enorm_tests(self):
        """Generate test cases for ENORM (Euclidean norm with overflow/underflow protection)"""
        tests = []
        
        # Category 1: Basic vectors where norm is obvious
        tests.append({
            "description": "Simple 3-4-5 triangle",
            "n": 2,
            "inputs": [3.0, 4.0],
            "expected": None
        })
        
        tests.append({
            "description": "Unit vector in 2D",
            "n": 2,
            "inputs": [1.0/math.sqrt(2), 1.0/math.sqrt(2)],
            "expected": None
        })
        
        tests.append({
            "description": "Unit vector in 3D",
            "n": 3,
            "inputs": [1.0/math.sqrt(3), 1.0/math.sqrt(3), 1.0/math.sqrt(3)],
            "expected": None
        })
        
        tests.append({
            "description": "All ones vector length 5",
            "n": 5,
            "inputs": [1.0, 1.0, 1.0, 1.0, 1.0],
            "expected": None
        })
        
        # Category 2: Edge cases
        tests.append({
            "description": "Empty vector (N=0)",
            "n": 0,
            "inputs": [],
            "expected": None
        })
        
        tests.append({
            "description": "Single element vector",
            "n": 1,
            "inputs": [5.0],
            "expected": None
        })
        
        tests.append({
            "description": "Single negative element",
            "n": 1,
            "inputs": [-7.0],
            "expected": None
        })
        
        tests.append({
            "description": "All zeros vector",
            "n": 4,
            "inputs": [0.0, 0.0, 0.0, 0.0],
            "expected": None
        })
        
        tests.append({
            "description": "Mixed signs",
            "n": 4,
            "inputs": [1.0, -2.0, 3.0, -4.0],
            "expected": None
        })
        
        # Category 3: Underflow protection (values near RDWARF = 3.834e-20)
        rdwarf = 3.834e-20
        
        tests.append({
            "description": "Single tiny value at RDWARF",
            "n": 1,
            "inputs": [rdwarf],
            "expected": None
        })
        
        tests.append({
            "description": "Two tiny values at RDWARF",
            "n": 2,
            "inputs": [rdwarf, rdwarf],
            "expected": None
        })
        
        tests.append({
            "description": "Values just below RDWARF",
            "n": 3,
            "inputs": [rdwarf * 0.5, rdwarf * 0.5, rdwarf * 0.5],
            "expected": None
        })
        
        tests.append({
            "description": "Values just above RDWARF",
            "n": 3,
            "inputs": [rdwarf * 2, rdwarf * 2, rdwarf * 2],
            "expected": None
        })
        
        # Category 4: Overflow protection (values near RGIANT = 1.304e19)
        rgiant = 1.304e19
        
        tests.append({
            "description": "Single huge value at RGIANT",
            "n": 1,
            "inputs": [rgiant],
            "expected": None
        })
        
        tests.append({
            "description": "Two huge values at RGIANT/2",
            "n": 2,
            "inputs": [rgiant/2, rgiant/2],
            "expected": None
        })
        
        tests.append({
            "description": "Values near overflow threshold",
            "n": 3,
            "inputs": [rgiant * 0.6, rgiant * 0.6, rgiant * 0.6],
            "expected": None
        })
        
        # Category 5: Mixed magnitudes
        tests.append({
            "description": "Mix of tiny and normal values",
            "n": 3,
            "inputs": [rdwarf, 1.0, 2.0],
            "expected": None
        })
        
        tests.append({
            "description": "Mix of huge and normal values",
            "n": 3,
            "inputs": [rgiant/10, 1.0, 2.0],
            "expected": None
        })
        
        tests.append({
            "description": "Mix of tiny, normal, and huge",
            "n": 5,
            "inputs": [rdwarf * 10, 0.1, 1.0, 10.0, rgiant/100],
            "expected": None
        })
        
        # Category 6: Scaling tests (same direction, different magnitudes)
        base_vec = [3.0, 4.0, 12.0]  # Norm = 13
        scales = [1e-10, 1e-5, 0.01, 0.1, 1, 10, 100, 1e5, 1e10]
        
        for scale in scales:
            scaled = [x * scale for x in base_vec]
            tests.append({
                "description": f"Vector [3,4,12] scaled by {scale}",
                "n": 3,
                "inputs": scaled,
                "expected": None
            })
        
        # Category 7: Random vectors of various sizes
        import random
        random.seed(42)  # For reproducibility
        
        # Small vectors
        for n in [1, 2, 3, 5, 10]:
            vec = [random.uniform(-10, 10) for _ in range(n)]
            tests.append({
                "description": f"Random vector size {n}",
                "n": n,
                "inputs": vec,
                "expected": None
            })
        
        # Medium vectors
        for n in [20, 30, 50]:
            vec = [random.uniform(-100, 100) for _ in range(n)]
            tests.append({
                "description": f"Random vector size {n}",
                "n": n,
                "inputs": vec,
                "expected": None
            })
        
        # Large vectors
        for n in [75, 100]:
            vec = [random.uniform(-1000, 1000) for _ in range(n)]
            tests.append({
                "description": f"Random vector size {n}",
                "n": n,
                "inputs": vec,
                "expected": None
            })
        
        # Special patterns
        tests.append({
            "description": "Alternating +1/-1 pattern",
            "n": 10,
            "inputs": [(-1)**i for i in range(10)],
            "expected": None
        })
        
        tests.append({
            "description": "Fibonacci sequence first 10",
            "n": 10,
            "inputs": [1.0, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0, 21.0, 34.0, 55.0],
            "expected": None
        })
        
        tests.append({
            "description": "Powers of 2",
            "n": 8,
            "inputs": [2**i for i in range(8)],
            "expected": None
        })
        
        # More underflow/overflow boundary tests
        tests.append({
            "description": "Vector with values spanning machine range",
            "n": 7,
            "inputs": [rdwarf*100, 1e-10, 1e-5, 1.0, 1e5, 1e10, rgiant/100],
            "expected": None
        })
        
        # Stress tests with extreme values
        tests.append({
            "description": "Many tiny values that sum to normal",
            "n": 100,
            "inputs": [1e-10] * 100,  # sqrt(100 * 1e-20) = 1e-9
            "expected": None
        })
        
        tests.append({
            "description": "Geometric progression",
            "n": 10,
            "inputs": [2**(-i) for i in range(10)],
            "expected": None
        })
        
        # Additional edge cases
        tests.append({
            "description": "Single zero with non-zeros",
            "n": 5,
            "inputs": [1.0, 2.0, 0.0, 3.0, 4.0],
            "expected": None
        })
        
        tests.append({
            "description": "Very large N with small values",
            "n": 100,
            "inputs": [0.01] * 100,
            "expected": None
        })
        
        # Numerical stability tests
        tests.append({
            "description": "Values that might cause intermediate overflow",
            "n": 3,
            "inputs": [1e15, 1e15, 1e15],
            "expected": None
        })
        
        tests.append({
            "description": "Values that might cause intermediate underflow",
            "n": 3,
            "inputs": [1e-15, 1e-15, 1e-15],
            "expected": None
        })
        
        # More mixed magnitude tests
        for exp_diff in [10, 20, 30]:
            tests.append({
                "description": f"Two values differing by 10^{exp_diff}",
                "n": 2,
                "inputs": [1.0, 10**(-exp_diff)],
                "expected": None
            })
        
        # Test near the boundary between small/intermediate/large
        agiant_approx = rgiant / 10  # Approximate AGIANT for N=10
        
        tests.append({
            "description": "Values just below AGIANT threshold",
            "n": 10,
            "inputs": [agiant_approx * 0.9] * 10,
            "expected": None
        })
        
        tests.append({
            "description": "Values just above AGIANT threshold",
            "n": 10,
            "inputs": [agiant_approx * 1.1] * 10,
            "expected": None
        })
        
        # Additional tests to reach 150-200 range
        
        # More Pythagorean-like triples at various scales
        pythagorean_triples = [
            (5, 12, 13), (8, 15, 17), (7, 24, 25), (20, 21, 29),
            (9, 40, 41), (11, 60, 61), (12, 35, 37), (13, 84, 85)
        ]
        for a, b, c in pythagorean_triples:
            for scale in [1e-15, 1e-8, 1e-3, 1e3, 1e8, 1e15]:
                tests.append({
                    "description": f"Pythagorean triple ({a},{b}) scaled by {scale:.0e}",
                    "n": 2,
                    "inputs": [a*scale, b*scale],
                    "expected": None
                })
        
        # Test vectors with increasing number of elements
        for n in [15, 25, 40, 60, 80]:
            # All equal values
            tests.append({
                "description": f"All equal values, N={n}",
                "n": n,
                "inputs": [1.0/math.sqrt(n)] * n,
                "expected": None
            })
            
            # Decreasing values
            tests.append({
                "description": f"Decreasing values 1/i, N={n}",
                "n": n,
                "inputs": [1.0/i for i in range(1, n+1)],
                "expected": None
            })
            
            # Random normal distribution
            import random
            random.seed(1234 + n)  # Different seed for each n
            tests.append({
                "description": f"Random normal distribution, N={n}",
                "n": n,
                "inputs": [random.gauss(0, 1) for _ in range(n)],
                "expected": None
            })
        
        # Edge cases with zeros interspersed
        tests.append({
            "description": "Alternating zeros and ones",
            "n": 20,
            "inputs": [i % 2 for i in range(20)],
            "expected": None
        })
        
        tests.append({
            "description": "Mostly zeros with few non-zeros",
            "n": 50,
            "inputs": [1.0 if i in [5, 15, 25, 35, 45] else 0.0 for i in range(50)],
            "expected": None
        })
        
        # Extreme scaling tests
        extreme_scales = [1e-30, 1e-25, 1e-22, 1e22, 1e25, 1e30]
        for scale in extreme_scales:
            tests.append({
                "description": f"Unit vector scaled by {scale:.0e}",
                "n": 3,
                "inputs": [scale/math.sqrt(3)] * 3,
                "expected": None
            })
        
        # Tests with specific patterns
        tests.append({
            "description": "Harmonic series first 30 terms",
            "n": 30,
            "inputs": [1.0/i for i in range(1, 31)],
            "expected": None
        })
        
        tests.append({
            "description": "Prime numbers as floats",
            "n": 25,
            "inputs": [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0, 19.0, 23.0, 29.0,
                      31.0, 37.0, 41.0, 43.0, 47.0, 53.0, 59.0, 61.0, 67.0, 71.0,
                      73.0, 79.0, 83.0, 89.0, 97.0],
            "expected": None
        })
        
        # Tests around machine precision boundaries
        tests.append({
            "description": "Values near machine epsilon",
            "n": 5,
            "inputs": [1.0, 1.0 + 1e-7, 1.0 - 1e-7, 1.0 + 1e-8, 1.0 - 1e-8],
            "expected": None
        })
        
        # More underflow/overflow combination tests
        tests.append({
            "description": "Extreme range: tiny to huge values",
            "n": 9,
            "inputs": [rdwarf, rdwarf*10, rdwarf*100, 1e-10, 1.0, 1e10, rgiant/100, rgiant/10, rgiant],
            "expected": None
        })
        
        # Sinusoidal patterns
        for freq in [1, 2, 5, 10]:
            n = 36
            tests.append({
                "description": f"Sinusoidal pattern frequency {freq}",
                "n": n,
                "inputs": [math.sin(freq * 2 * math.pi * i / n) for i in range(n)],
                "expected": None
            })
        
        # Exponential growth/decay patterns
        tests.append({
            "description": "Exponential growth pattern",
            "n": 20,
            "inputs": [math.exp(i/5.0) for i in range(20)],
            "expected": None
        })
        
        tests.append({
            "description": "Exponential decay pattern",
            "n": 20,
            "inputs": [math.exp(-i/5.0) for i in range(20)],
            "expected": None
        })
        
        # Tests with specific norms
        # Norm = 10 with different distributions
        tests.append({
            "description": "Two values with norm 10",
            "n": 2,
            "inputs": [6.0, 8.0],
            "expected": None
        })
        
        tests.append({
            "description": "Three values with norm ~10",
            "n": 3,
            "inputs": [5.0, 7.0, 5.0],
            "expected": None
        })
        
        tests.append({
            "description": "Ten values with norm ~10",
            "n": 10,
            "inputs": [10.0/math.sqrt(10)] * 10,
            "expected": None
        })
        
        # Complex patterns
        tests.append({
            "description": "Alternating large positive/negative values",
            "n": 20,
            "inputs": [1000.0 * ((-1)**i) for i in range(20)],
            "expected": None
        })
        
        # More random tests with different distributions
        random.seed(9876)
        
        # Uniform distribution
        for n in [35, 45, 65, 85]:
            tests.append({
                "description": f"Random uniform [0,1], N={n}",
                "n": n,
                "inputs": [random.uniform(0, 1) for _ in range(n)],
                "expected": None
            })
        
        # Exponential distribution
        for n in [33, 47, 73]:
            tests.append({
                "description": f"Random exponential distribution, N={n}",
                "n": n,
                "inputs": [random.expovariate(1.0) for _ in range(n)],
                "expected": None
            })
        
        # Log-normal distribution
        tests.append({
            "description": "Log-normal distribution N=40",
            "n": 40,
            "inputs": [random.lognormvariate(0, 1) for _ in range(40)],
            "expected": None
        })
        
        # Tests with exact integer values
        tests.append({
            "description": "Consecutive integers 1 to 30",
            "n": 30,
            "inputs": [float(i) for i in range(1, 31)],
            "expected": None
        })
        
        tests.append({
            "description": "Perfect squares",
            "n": 15,
            "inputs": [float(i*i) for i in range(1, 16)],
            "expected": None
        })
        
        # Edge case: very long vectors
        tests.append({
            "description": "Very long vector N=100 with pattern",
            "n": 100,
            "inputs": [(i % 10 + 1) * 0.1 for i in range(100)],
            "expected": None
        })
        
        # Boundary test cases for overflow/underflow thresholds
        # Test AGIANT = RGIANT/N for various N
        for n in [2, 5, 10, 20, 50, 100]:
            agiant = rgiant / n
            tests.append({
                "description": f"At AGIANT boundary for N={n}",
                "n": n,
                "inputs": [agiant] * n,
                "expected": None
            })
        
        # Additional special values tests
        tests.append({
            "description": "Mix of special float values",
            "n": 7,
            "inputs": [0.0, -0.0, 1.0, -1.0, 0.5, -0.5, 2.0],
            "expected": None
        })
        
        # Tests that stress the algorithm's three different sums
        tests.append({
            "description": "Values in all three ranges",
            "n": 12,
            "inputs": [
                rdwarf/2, rdwarf, rdwarf*2,  # Small
                0.001, 0.1, 1.0, 10.0,       # Intermediate  
                rgiant/10, rgiant/5, rgiant/2, rgiant*0.8, rgiant  # Large
            ],
            "expected": None
        })
        
        print(f"Generated {len(tests)} test cases for ENORM")
        return tests
    
    def _generate_lsame_tests(self):
        """Generate test cases for LSAME (case-insensitive character comparison)"""
        tests = []
        
        # Category 1: Basic same letter tests
        tests.append({
            "description": "Same uppercase letters",
            "inputs": ["A", "A"],
            "expected": None
        })
        
        tests.append({
            "description": "Same lowercase letters",
            "inputs": ["a", "a"],
            "expected": None
        })
        
        tests.append({
            "description": "Lowercase vs uppercase (same letter)",
            "inputs": ["a", "A"],
            "expected": None
        })
        
        tests.append({
            "description": "Uppercase vs uppercase (same letter)",
            "inputs": ["A", "A"],
            "expected": None
        })
        
        # Category 2: Different letters
        tests.append({
            "description": "Different uppercase letters",
            "inputs": ["A", "B"],
            "expected": None
        })
        
        tests.append({
            "description": "Different lowercase letters",
            "inputs": ["a", "b"],
            "expected": None
        })
        
        tests.append({
            "description": "Different letters mixed case",
            "inputs": ["a", "B"],
            "expected": None
        })
        
        # Category 3: All alphabet letters
        # Test all letters against themselves (uppercase)
        for i in range(26):
            letter = chr(ord('A') + i)
            tests.append({
                "description": f"Letter {letter} uppercase vs uppercase",
                "inputs": [letter, letter],
                "expected": None
            })
        
        # Test all letters against themselves (lowercase vs uppercase)
        for i in range(26):
            lower = chr(ord('a') + i)
            upper = chr(ord('A') + i)
            tests.append({
                "description": f"Letter {lower} lowercase vs {upper} uppercase",
                "inputs": [lower, upper],
                "expected": None
            })
        
        # Category 4: Non-letter characters
        special_chars = ['0', '1', '9', ' ', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '[', ']', '{', '}', ';', ':', "'", '"', ',', '.', '<', '>', '/', '?', '\\', '|']
        for char in special_chars:
            tests.append({
                "description": f"Special character '{char}' vs 'A'",
                "inputs": [char, "A"],
                "expected": None
            })
        
        # Test special characters against themselves
        for char in special_chars[:10]:  # Just first 10 to keep test count reasonable
            tests.append({
                "description": f"Special character '{char}' vs itself",
                "inputs": [char, char],
                "expected": None
            })
        
        # Category 5: Edge cases with adjacent letters
        adjacent_pairs = [('A', 'B'), ('B', 'C'), ('Y', 'Z'), ('a', 'b'), ('y', 'z')]
        for char1, char2 in adjacent_pairs:
            tests.append({
                "description": f"Adjacent letters {char1} vs {char2}",
                "inputs": [char1, char2],
                "expected": None
            })
        
        # Category 6: Case variations for specific letters
        test_letters = ['A', 'Z', 'M', 'a', 'z', 'm']
        for letter1 in test_letters:
            for letter2 in test_letters:
                tests.append({
                    "description": f"Case test: '{letter1}' vs '{letter2}'",
                    "inputs": [letter1, letter2],
                    "expected": None
                })
        
        # Category 7: Numbers and digits
        for digit in '0123456789':
            tests.append({
                "description": f"Digit '{digit}' vs 'A'",
                "inputs": [digit, "A"],
                "expected": None
            })
            tests.append({
                "description": f"Digit '{digit}' vs itself",
                "inputs": [digit, digit],
                "expected": None
            })
        
        print(f"Generated {len(tests)} test cases for LSAME")
        return tests
    
    def run_f77_reference(self, test_cases):
        """Run F77 implementation to get reference values"""
        all_results = []
        
        for batch_start in range(0, len(test_cases), self.batch_size):
            batch_end = min(batch_start + self.batch_size, len(test_cases))
            batch = test_cases[batch_start:batch_end]
            
            # Generate F77 program for this batch
            program = self._generate_f77_program(batch, batch_start)
            
            # Write, compile, run
            test_file = "temp_test.f"
            with open(test_file, 'w') as f:
                f.write(program)
            
            exe_file = "temp_test"
            # Special case for machine constants - use IEEE version
            if self.func_name == "I1MACH":
                src_file = "src/i1mach_ieee.f"
            elif self.func_name == "R1MACH":
                src_file = "src/r1mach_ieee.f"
            elif self.func_name == "D1MACH":
                src_file = "src/d1mach_ieee.f"
            else:
                src_file = f"src/{self.func_name.lower()}.f"
            
            # Compile
            compile_result = subprocess.run(
                ['gfortran', '-o', exe_file, test_file, src_file],
                capture_output=True, text=True
            )
            
            if compile_result.returncode != 0:
                print(f"Compilation failed: {compile_result.stderr}")
                return None
            
            # Run
            run_result = subprocess.run(
                [f'./{exe_file}'], capture_output=True, text=True
            )
            
            if run_result.returncode != 0:
                print(f"Execution failed: {run_result.stderr}")
                return None
            
            # Parse results
            results = self._parse_f77_output(run_result.stdout)
            all_results.extend(results)
            
            # Cleanup
            Path(test_file).unlink(missing_ok=True)
            Path(exe_file).unlink(missing_ok=True)
            
            print(f"Batch {batch_start+1}-{batch_end}: {len(results)} results")
        
        return all_results
    
    def _generate_f77_program(self, test_cases, start_index):
        """Generate F77 test program based on function signature"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_f77(test_cases, start_index)
        elif self.func_name == "CDIV":
            return self._generate_cdiv_f77(test_cases, start_index)
        elif self.func_name == "I1MACH":
            return self._generate_i1mach_f77(test_cases, start_index)
        elif self.func_name == "R1MACH":
            return self._generate_r1mach_f77(test_cases, start_index)
        elif self.func_name == "D1MACH":
            return self._generate_d1mach_f77(test_cases, start_index)
        elif self.func_name == "ENORM":
            return self._generate_enorm_f77(test_cases, start_index)
        elif self.func_name == "LSAME":
            return self._generate_lsame_f77(test_cases, start_index)
        else:
            raise NotImplementedError(f"No F77 generator for {self.func_name}")
    
    def _generate_pythag_f77(self, test_cases, start_index):
        """Generate F77 program for PYTHAG"""
        program = f"""      PROGRAM TEST_PYTHAG
      REAL PYTHAG, A, B, RESULT
      EXTERNAL PYTHAG
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            a, b = test['inputs']
            program += f"""C     Test {test_num}
      A = {a:e}
      B = {b:e}
      RESULT = PYTHAG(A, B)
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_cdiv_f77(self, test_cases, start_index):
        """Generate F77 program for CDIV"""
        program = f"""      PROGRAM TEST_CDIV
      REAL AR, AI, BR, BI, CR, CI
      EXTERNAL CDIV
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            ar, ai, br, bi = test['inputs']
            program += f"""C     Test {test_num}
      AR = {ar:e}
      AI = {ai:e}
      BR = {br:e}
      BI = {bi:e}
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      WRITE(*,'(A,I5,A,E20.10,A,E20.10)') 'TEST_', {test_num}, 
     +    '_RESULT: ', CR, ', ', CI
      
"""
        program += "      END"
        return program
    
    def _generate_i1mach_f77(self, test_cases, start_index):
        """Generate F77 program for I1MACH"""
        program = f"""      PROGRAM TEST_I1MACH
      INTEGER I1MACH, I, RESULT
      EXTERNAL I1MACH
      
"""
        for idx, test in enumerate(test_cases):
            test_num = start_index + idx + 1
            i = test['inputs'][0]
            program += f"""C     Test {test_num}
      I = {i}
      RESULT = I1MACH(I)
      WRITE(*,'(A,I5,A,I15)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_r1mach_f77(self, test_cases, start_index):
        """Generate F77 program for R1MACH"""
        program = f"""      PROGRAM TEST_R1MACH
      REAL R1MACH, RESULT
      INTEGER I
      EXTERNAL R1MACH
      
"""
        for idx, test in enumerate(test_cases):
            test_num = start_index + idx + 1
            i = test['inputs'][0]
            program += f"""C     Test {test_num}
      I = {i}
      RESULT = R1MACH(I)
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_d1mach_f77(self, test_cases, start_index):
        """Generate F77 program for D1MACH"""
        program = f"""      PROGRAM TEST_D1MACH
      DOUBLE PRECISION D1MACH, RESULT
      INTEGER I
      EXTERNAL D1MACH
      
"""
        for idx, test in enumerate(test_cases):
            test_num = start_index + idx + 1
            i = test['inputs'][0]
            program += f"""C     Test {test_num}
      I = {i}
      RESULT = D1MACH(I)
      WRITE(*,'(A,I5,A,D25.16)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_enorm_f77(self, test_cases, start_index):
        """Generate F77 program for ENORM"""
        program = f"""      PROGRAM TEST_ENORM
      REAL ENORM, RESULT
      EXTERNAL ENORM
      INTEGER N
      REAL X(100)
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            n = test['n']
            values = test['inputs']
            
            program += f"""C     Test {test_num}
      N = {n}
"""
            # Initialize array
            for j, val in enumerate(values):
                program += f"      X({j+1}) = {val:e}\n"
            
            program += f"""      RESULT = ENORM(N, X)
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_lsame_f77(self, test_cases, start_index):
        """Generate F77 program for LSAME"""
        program = f"""      PROGRAM TEST_LSAME
      LOGICAL LSAME, RESULT
      EXTERNAL LSAME
      CHARACTER*1 CA, CB
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            ca, cb = test['inputs']
            # Need to escape single quotes in F77 character constants
            ca_escaped = ca.replace("'", "''")
            cb_escaped = cb.replace("'", "''")
            
            program += f"""C     Test {test_num}
      CA = '{ca_escaped}'
      CB = '{cb_escaped}'
      RESULT = LSAME(CA, CB)
      IF (RESULT) THEN
        WRITE(*,'(A,I5,A)') 'TEST_', {test_num}, '_RESULT: T'
      ELSE
        WRITE(*,'(A,I5,A)') 'TEST_', {test_num}, '_RESULT: F'
      ENDIF
      
"""
        program += "      END"
        return program
    
    def _parse_f77_output(self, output):
        """Parse F77 output to extract results"""
        results = []
        
        if self.func_name == "PYTHAG":
            # Single result per test
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = float(match.group(2))
                results.append((test_num, value))
                
        elif self.func_name == "CDIV":
            # Two results per test (real, imaginary)
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+),\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                real_part = float(match.group(2))
                imag_part = float(match.group(3))
                results.append((test_num, real_part, imag_part))
                
        elif self.func_name == "I1MACH":
            # Integer result per test
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = int(match.group(2))
                results.append((test_num, value))
                
        elif self.func_name == "R1MACH":
            # Single precision float result per test
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = float(match.group(2))
                results.append((test_num, value))
                
        elif self.func_name == "D1MACH":
            # Double precision float result per test (may have D, E, or no letter before exponent)
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+(?:[DdEe]?[-+]\d+)?)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value_str = match.group(2)
                # Replace D with E for Python's float parser
                value_str = value_str.replace('D', 'E').replace('d', 'E')
                # Handle case where exponent has no E/D (e.g., 0.123-307)
                import re as regex
                if regex.search(r'\d[-+]\d', value_str):
                    # Insert E before the exponent
                    value_str = regex.sub(r'(\d)([-+]\d)', r'\1E\2', value_str)
                # Special handling for values that overflow Python's float
                # D1MACH(2) prints as 0.179...E+309 which is actually 1.79...E+308
                if 'E+309' in value_str or 'e+309' in value_str:
                    # For D1MACH(2), we know the exact IEEE value
                    value = 1.7976931348623157e+308
                else:
                    value = float(value_str)
                results.append((test_num, value))
                
        elif self.func_name == "ENORM":
            # Single result per test
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = float(match.group(2))
                results.append((test_num, value))
                
        elif self.func_name == "LSAME":
            # Boolean result per test (T or F)
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([TF])'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = match.group(2) == 'T'  # Convert to Python boolean
                results.append((test_num, value))
        
        return results
    
    def save_test_data(self, test_cases, results):
        """Save test cases with reference values"""
        # Update test cases with results
        if self.func_name == "PYTHAG":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "CDIV":
            for (test_num, real_part, imag_part), test_case in zip(results, test_cases):
                test_case['expected'] = [real_part, imag_part]
                test_case['test_id'] = test_num
        elif self.func_name == "I1MACH":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "R1MACH":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "D1MACH":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "ENORM":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "LSAME":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        
        # Create output structure
        output_data = {
            "function": self.func_name.lower(),
            "signature": self._get_signature(),
            "description": self._get_description(),
            "total_tests": len(test_cases),
            "test_cases": test_cases
        }
        
        # Ensure test_data directory exists
        Path("test_data").mkdir(exist_ok=True)
        
        # Save
        with open(self.test_file, 'w') as f:
            json.dump(output_data, f, indent=2)
        
        print(f"Saved {len(test_cases)} test cases to {self.test_file}")
    
    def validate_modern(self):
        """Validate modern implementation against test data"""
        # Load test data
        with open(self.test_file, 'r') as f:
            test_data = json.load(f)
        
        test_cases = test_data['test_cases']
        print(f"Validating {len(test_cases)} test cases...")
        
        # Run modern implementation
        results = self._run_modern_implementation(test_cases)
        
        if not results:
            print("Failed to run modern implementation")
            return False
        
        # Compare results
        tolerance = 1e-6
        failures = 0
        
        for result, test_case in zip(results, test_cases):
            if self.func_name == "PYTHAG":
                actual = result[1]  # (test_num, value)
                expected = test_case['expected']
                
                if expected != 0:
                    rel_error = abs(actual - expected) / abs(expected)
                else:
                    rel_error = abs(actual - expected)
                
                if rel_error > tolerance:
                    failures += 1
                    if failures <= 5:
                        print(f"\nTest {result[0]} FAILED:")
                        print(f"  Expected: {expected}")
                        print(f"  Actual: {actual}")
                        print(f"  Error: {rel_error}")
            elif self.func_name == "I1MACH":
                actual = result[1]  # (test_num, value)
                expected = test_case['expected']
                
                # For integers, must match exactly
                if actual != expected:
                    failures += 1
                    if failures <= 5:
                        print(f"\nTest {result[0]} FAILED:")
                        print(f"  Description: {test_case['description']}")
                        print(f"  Expected: {expected}")
                        print(f"  Actual: {actual}")
            elif self.func_name == "R1MACH":
                actual = result[1]  # (test_num, value)
                expected = test_case['expected']
                
                if expected != 0:
                    rel_error = abs(actual - expected) / abs(expected)
                else:
                    rel_error = abs(actual - expected)
                
                if rel_error > tolerance:
                    failures += 1
                    if failures <= 5:
                        print(f"\nTest {result[0]} FAILED:")
                        print(f"  Description: {test_case['description']}")
                        print(f"  Expected: {expected}")
                        print(f"  Actual: {actual}")
                        print(f"  Error: {rel_error}")
                        
            elif self.func_name == "D1MACH":
                actual = result[1]  # (test_num, value)
                expected = test_case['expected']
                
                if expected != 0:
                    rel_error = abs(actual - expected) / abs(expected)
                else:
                    rel_error = abs(actual - expected)
                
                if rel_error > tolerance:
                    failures += 1
                    if failures <= 5:
                        print(f"\nTest {result[0]} FAILED:")
                        print(f"  Description: {test_case['description']}")
                        print(f"  Expected: {expected}")
                        print(f"  Actual: {actual}")
                        print(f"  Error: {rel_error}")
                        
        print(f"\n{len(test_cases) - failures} tests PASSED")
        print(f"{failures} tests FAILED")
        
        return failures == 0
    
    def _run_modern_implementation(self, test_cases):
        """Run modern F90 implementation"""
        # Generate F90 test program
        program = self._generate_modern_test_program(test_cases)
        
        test_file = "test_modern.f90"
        with open(test_file, 'w') as f:
            f.write(program)
        
        module_file = f"modern/{self.func_name.lower()}_modern.f90"
        exe_file = "test_modern"
        
        # Compile
        compile_result = subprocess.run(
            ['gfortran', '-o', exe_file, module_file, test_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"Compilation failed: {compile_result.stderr}")
            return None
        
        # Run
        run_result = subprocess.run(
            [f'./{exe_file}'], capture_output=True, text=True
        )
        
        if run_result.returncode != 0:
            print(f"Execution failed: {run_result.stderr}")
            return None
        
        # Parse and cleanup
        results = self._parse_f77_output(run_result.stdout)
        
        Path(test_file).unlink(missing_ok=True)
        Path(exe_file).unlink(missing_ok=True)
        Path(f"{self.func_name.lower()}_module.mod").unlink(missing_ok=True)
        
        return results
    
    def _generate_modern_test_program(self, test_cases):
        """Generate modern F90 test program"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_modern_test(test_cases)
        elif self.func_name == "CDIV":
            return self._generate_cdiv_modern_test(test_cases)
        elif self.func_name == "I1MACH":
            return self._generate_i1mach_modern_test(test_cases)
        elif self.func_name == "R1MACH":
            return self._generate_r1mach_modern_test(test_cases)
        elif self.func_name == "D1MACH":
            return self._generate_d1mach_modern_test(test_cases)
        else:
            raise NotImplementedError(f"No modern test generator for {self.func_name}")
    
    def _generate_pythag_modern_test(self, test_cases):
        """Generate modern F90 test for PYTHAG"""
        program = f"""program test_pythag
    use pythag_module, only: pythag
    implicit none
    
    real :: a, b, result
    
"""
        for i, test in enumerate(test_cases[:self.batch_size]):  # Limit batch size
            a, b = test['inputs']
            program += f"""    ! Test {i+1}
    a = {a:e}
    b = {b:e}
    result = pythag(a, b)
    write(*,'(A,I5,A,E20.10)') 'TEST_', {i+1}, '_RESULT: ', result
    
"""
        program += "end program test_pythag"
        return program
    
    def _generate_cdiv_modern_test(self, test_cases):
        """Generate modern F90 test for CDIV"""
        program = f"""program test_cdiv
    use cdiv_module, only: cdiv
    implicit none
    
    real :: ar, ai, br, bi, cr, ci
    
"""
        for i, test in enumerate(test_cases[:self.batch_size]):
            ar, ai, br, bi = test['inputs']
            program += f"""    ! Test {i+1}
    ar = {ar:e}
    ai = {ai:e}
    br = {br:e}
    bi = {bi:e}
    call cdiv(ar, ai, br, bi, cr, ci)
    write(*,'(A,I5,A,E20.10,A,E20.10)') 'TEST_', {i+1}, &
        '_RESULT: ', cr, ', ', ci
    
"""
        program += "end program test_cdiv"
        return program
    
    def _generate_i1mach_modern_test(self, test_cases):
        """Generate modern F90 test for I1MACH"""
        program = f"""program test_i1mach
    use i1mach_module, only: i1mach
    implicit none
    
    integer :: i, result
    
"""
        for idx, test in enumerate(test_cases[:self.batch_size]):
            i_val = test['inputs'][0]
            program += f"""    ! Test {idx+1}
    i = {i_val}
    result = i1mach(i)
    write(*,'(A,I5,A,I15)') 'TEST_', {idx+1}, '_RESULT: ', result
    
"""
        program += "end program test_i1mach"
        return program
    
    def _generate_r1mach_modern_test(self, test_cases):
        """Generate modern F90 test for R1MACH"""
        program = f"""program test_r1mach
    use r1mach_module, only: r1mach
    implicit none
    
    real :: result
    integer :: i
    
"""
        for idx, test in enumerate(test_cases[:self.batch_size]):
            i_val = test['inputs'][0]
            program += f"""    ! Test {idx+1}
    i = {i_val}
    result = r1mach(i)
    write(*,'(A,I5,A,E20.10)') 'TEST_', {idx+1}, '_RESULT: ', result
    
"""
        program += "end program test_r1mach"
        return program
    
    def _generate_d1mach_modern_test(self, test_cases):
        """Generate modern F90 test for D1MACH"""
        program = f"""program test_d1mach
    use d1mach_module, only: d1mach
    implicit none
    
    double precision :: result
    integer :: i
    
"""
        for idx, test in enumerate(test_cases[:self.batch_size]):
            i_val = test['inputs'][0]
            program += f"""    ! Test {idx+1}
    i = {i_val}
    result = d1mach(i)
    write(*,'(A,I5,A,D25.16)') 'TEST_', {idx+1}, '_RESULT: ', result
    
"""
        program += "end program test_d1mach"
        return program
    
    def _get_signature(self):
        """Get function signature"""
        signatures = {
            "PYTHAG": "REAL FUNCTION PYTHAG(A, B)",
            "CDIV": "SUBROUTINE CDIV(AR, AI, BR, BI, CR, CI)",
            "I1MACH": "INTEGER FUNCTION I1MACH(I)",
            "R1MACH": "REAL FUNCTION R1MACH(I)",
            "D1MACH": "DOUBLE PRECISION FUNCTION D1MACH(I)",
            "ENORM": "REAL FUNCTION ENORM(N, X)"
        }
        return signatures.get(self.func_name, "Unknown")
    
    def _get_description(self):
        """Get function description"""
        descriptions = {
            "PYTHAG": "Compute sqrt(a^2 + b^2) without overflow",
            "CDIV": "Complex division: (CR,CI) = (AR,AI)/(BR,BI)",
            "I1MACH": "Return integer machine dependent constants",
            "R1MACH": "Return floating point machine dependent constants",
            "D1MACH": "Return double precision machine dependent constants",
            "ENORM": "Compute Euclidean norm of a vector with overflow/underflow protection"
        }
        return descriptions.get(self.func_name, "No description")


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    
    command = sys.argv[1]
    func_name = sys.argv[2]
    
    helper = SlatecTestHelper(func_name)
    
    if command == "generate":
        # Generate test cases
        test_cases = helper.generate_test_cases()
        print(f"Generated {len(test_cases)} test cases")
        
        # Run F77 to get reference values
        results = helper.run_f77_reference(test_cases)
        if results:
            helper.save_test_data(test_cases, results)
        
    elif command == "validate":
        # Validate modern implementation
        success = helper.validate_modern()
        sys.exit(0 if success else 1)
        
    else:
        print(f"Unknown command: {command}")
        print("Use: generate, validate")
        sys.exit(1)


if __name__ == "__main__":
    main()