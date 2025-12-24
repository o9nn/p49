/**
 * Validate AIML XML files
 */
import * as fs from 'fs';
import * as path from 'path';

function validateXml(filePath: string): boolean {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');
    
    // Basic XML structure checks
    if (!content.includes('<?xml version="1.0" encoding="UTF-8"?>')) {
      console.log(`  ✗ Missing XML declaration`);
      return false;
    }
    
    if (!content.includes('<aiml version="2.0">')) {
      console.log(`  ✗ Missing AIML root element`);
      return false;
    }
    
    if (!content.includes('</aiml>')) {
      console.log(`  ✗ Missing closing AIML tag`);
      return false;
    }
    
    // Count opening and closing tags
    const categoryOpen = (content.match(/<category>/g) || []).length;
    const categoryClose = (content.match(/<\/category>/g) || []).length;
    const patternOpen = (content.match(/<pattern>/g) || []).length;
    const patternClose = (content.match(/<\/pattern>/g) || []).length;
    const templateOpen = (content.match(/<template>/g) || []).length;
    const templateClose = (content.match(/<\/template>/g) || []).length;
    
    if (categoryOpen !== categoryClose) {
      console.log(`  ✗ Mismatched category tags: ${categoryOpen} open, ${categoryClose} close`);
      return false;
    }
    
    if (patternOpen !== patternClose) {
      console.log(`  ✗ Mismatched pattern tags: ${patternOpen} open, ${patternClose} close`);
      return false;
    }
    
    if (templateOpen !== templateClose) {
      console.log(`  ✗ Mismatched template tags: ${templateOpen} open, ${templateClose} close`);
      return false;
    }
    
    // Check for proper escaping
    const unescapedAmp = content.match(/&(?!(amp|lt|gt|quot|apos);)/g);
    if (unescapedAmp && unescapedAmp.length > 0) {
      console.log(`  ⚠️  Warning: Possibly unescaped & characters found`);
    }
    
    console.log(`  ✓ Valid (${categoryOpen} categories, ${patternOpen} patterns)`);
    return true;
    
  } catch (error) {
    console.log(`  ✗ Error reading file: ${error}`);
    return false;
  }
}

function main() {
  console.log('Validating AIML files...\n');
  
  const aimlDir = '/home/runner/work/p49/p49/aiml';
  const files = fs.readdirSync(aimlDir).filter(f => f.endsWith('.aiml')).sort();
  
  let allValid = true;
  let totalCategories = 0;
  let totalPatterns = 0;
  
  files.forEach(file => {
    console.log(`${file}`);
    const filePath = path.join(aimlDir, file);
    const isValid = validateXml(filePath);
    if (!isValid) allValid = false;
    
    // Count categories and patterns
    const content = fs.readFileSync(filePath, 'utf-8');
    const categories = (content.match(/<category>/g) || []).length;
    const patterns = (content.match(/<pattern>/g) || []).length;
    totalCategories += categories;
    totalPatterns += patterns;
  });
  
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Total files: ${files.length}`);
  console.log(`Total categories: ${totalCategories}`);
  console.log(`Total patterns: ${totalPatterns}`);
  console.log(`All valid: ${allValid ? '✓ YES' : '✗ NO'}`);
  console.log(`${'='.repeat(60)}\n`);
  
  process.exit(allValid ? 0 : 1);
}

main();
