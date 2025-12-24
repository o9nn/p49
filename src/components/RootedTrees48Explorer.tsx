/**
 * RootedTrees48Explorer.tsx
 * Interactive visualization of the 48 rooted trees and Pattern Dynamics mapping
 */

import React, { useState, useMemo } from 'react';
import {
  generateComplete48TreeMapping,
  getSummaryStatistics,
  type TreeAnalysis,
  type PatternFamily,
  type TreeShape,
} from '../lib/rooted-trees-48.data';

export function RootedTrees48Explorer() {
  const [selectedTree, setSelectedTree] = useState<TreeAnalysis | null>(null);
  const [familyFilter, setFamilyFilter] = useState<PatternFamily | 'all'>('all');
  const [shapeFilter, setShapeFilter] = useState<TreeShape | 'all'>('all');
  const [viewMode, setViewMode] = useState<'grid' | 'list'>('grid');

  // Generate mapping data
  const mapping = useMemo(() => generateComplete48TreeMapping(), []);
  const statistics = useMemo(
    () => getSummaryStatistics(mapping.trees),
    [mapping.trees]
  );

  // Filter trees
  const filteredTrees = useMemo(() => {
    let trees = mapping.trees;
    
    if (familyFilter !== 'all') {
      trees = trees.filter(t => t.patternFamily === familyFilter);
    }
    
    if (shapeFilter !== 'all') {
      trees = trees.filter(t => t.shape === shapeFilter);
    }
    
    return trees;
  }, [mapping.trees, familyFilter, shapeFilter]);

  // Get unique families and shapes for filters
  const families = useMemo(() => {
    const uniqueFamilies = new Set(mapping.trees.map(t => t.patternFamily));
    return Array.from(uniqueFamilies);
  }, [mapping.trees]);

  const shapes = useMemo(() => {
    const uniqueShapes = new Set(mapping.trees.map(t => t.shape));
    return Array.from(uniqueShapes);
  }, [mapping.trees]);

  return (
    <div className="w-full max-w-7xl mx-auto p-6 space-y-6">
      {/* Header */}
      <div className="bg-gradient-to-r from-emerald-50 to-teal-50 dark:from-emerald-950 dark:to-teal-950 rounded-lg p-6 border border-emerald-200 dark:border-emerald-800">
        <h1 className="text-3xl font-bold mb-2 text-emerald-900 dark:text-emerald-100">
          Rooted Trees 48 Explorer
        </h1>
        <p className="text-emerald-700 dark:text-emerald-300 mb-4">
          Explore the 48 distinct rooted trees with 7 nodes (OEIS A000081) and their correspondence to Pattern Dynamics
        </p>
        
        {/* 1+48=49 Structure */}
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-emerald-200 dark:border-emerald-700">
          <h3 className="font-semibold text-emerald-900 dark:text-emerald-100 mb-2">
            The 1+48=49 Correspondence
          </h3>
          <div className="grid md:grid-cols-2 gap-4 text-sm">
            <div>
              <p className="font-medium text-gray-700 dark:text-gray-300 mb-1">OEIS A000081:</p>
              <p className="text-gray-600 dark:text-gray-400">{mapping.structure.oeis.one}</p>
              <p className="text-gray-600 dark:text-gray-400">{mapping.structure.oeis.fortyEight}</p>
              <p className="text-gray-600 dark:text-gray-400 font-medium">{mapping.structure.oeis.total}</p>
            </div>
            <div>
              <p className="font-medium text-gray-700 dark:text-gray-300 mb-1">Pattern Dynamics:</p>
              <p className="text-gray-600 dark:text-gray-400">{mapping.structure.patterns.one}</p>
              <p className="text-gray-600 dark:text-gray-400">{mapping.structure.patterns.fortyEight}</p>
              <p className="text-gray-600 dark:text-gray-400 font-medium">{mapping.structure.patterns.total}</p>
            </div>
          </div>
        </div>
      </div>

      {/* Statistics Overview */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <p className="text-sm text-gray-600 dark:text-gray-400">Total Trees</p>
          <p className="text-3xl font-bold text-emerald-600 dark:text-emerald-400">{statistics.total}</p>
        </div>
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <p className="text-sm text-gray-600 dark:text-gray-400">Depth Range</p>
          <p className="text-2xl font-bold text-blue-600 dark:text-blue-400">
            {statistics.depths.min}–{statistics.depths.max}
          </p>
        </div>
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <p className="text-sm text-gray-600 dark:text-gray-400">Width Range</p>
          <p className="text-2xl font-bold text-purple-600 dark:text-purple-400">
            {statistics.widths.min}–{statistics.widths.max}
          </p>
        </div>
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <p className="text-sm text-gray-600 dark:text-gray-400">Nodes Each</p>
          <p className="text-3xl font-bold text-teal-600 dark:text-teal-400">7</p>
        </div>
      </div>

      {/* Filters and View Controls */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
              Pattern Family
            </label>
            <select
              value={familyFilter}
              onChange={(e) => setFamilyFilter(e.target.value as PatternFamily | 'all')}
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            >
              <option value="all">All Families</option>
              {families.map(family => (
                <option key={family} value={family}>
                  {family.replace('-', ' ').split(' ').map(w => 
                    w.charAt(0).toUpperCase() + w.slice(1)
                  ).join(' ')}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
              Tree Shape
            </label>
            <select
              value={shapeFilter}
              onChange={(e) => setShapeFilter(e.target.value as TreeShape | 'all')}
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            >
              <option value="all">All Shapes</option>
              {shapes.map(shape => (
                <option key={shape} value={shape}>
                  {shape.replace('-', ' ').split(' ').map(w => 
                    w.charAt(0).toUpperCase() + w.slice(1)
                  ).join(' ')}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
              View Mode
            </label>
            <div className="flex gap-2">
              <button
                onClick={() => setViewMode('grid')}
                className={`flex-1 px-4 py-2 rounded-md font-medium transition-colors ${
                  viewMode === 'grid'
                    ? 'bg-emerald-600 text-white'
                    : 'bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300'
                }`}
              >
                Grid
              </button>
              <button
                onClick={() => setViewMode('list')}
                className={`flex-1 px-4 py-2 rounded-md font-medium transition-colors ${
                  viewMode === 'list'
                    ? 'bg-emerald-600 text-white'
                    : 'bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300'
                }`}
              >
                List
              </button>
            </div>
          </div>
        </div>

        <div className="mt-4 text-sm text-gray-600 dark:text-gray-400">
          Showing {filteredTrees.length} of {statistics.total} trees
        </div>
      </div>

      {/* Trees Display */}
      {viewMode === 'grid' ? (
        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-6 gap-3">
          {filteredTrees.map((tree) => (
            <button
              key={tree.index}
              onClick={() => setSelectedTree(tree)}
              className={`p-3 rounded-lg border-2 transition-all hover:scale-105 ${
                selectedTree?.index === tree.index
                  ? 'border-emerald-500 bg-emerald-50 dark:bg-emerald-900'
                  : 'border-gray-200 dark:border-gray-700 bg-white dark:bg-gray-800'
              }`}
            >
              <div className="text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">
                Tree #{tree.index}
              </div>
              <div className="font-mono text-sm break-all text-gray-900 dark:text-gray-100 mb-2">
                {tree.string}
              </div>
              <div className="flex gap-1 text-xs">
                <span className="px-2 py-0.5 rounded bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300">
                  d:{tree.depth}
                </span>
                <span className="px-2 py-0.5 rounded bg-purple-100 dark:bg-purple-900 text-purple-700 dark:text-purple-300">
                  w:{tree.width}
                </span>
              </div>
            </button>
          ))}
        </div>
      ) : (
        <div className="space-y-2">
          {filteredTrees.map((tree) => (
            <button
              key={tree.index}
              onClick={() => setSelectedTree(tree)}
              className={`w-full p-4 rounded-lg border-2 text-left transition-all ${
                selectedTree?.index === tree.index
                  ? 'border-emerald-500 bg-emerald-50 dark:bg-emerald-900'
                  : 'border-gray-200 dark:border-gray-700 bg-white dark:bg-gray-800'
              }`}
            >
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-4">
                  <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
                    #{tree.index}
                  </span>
                  <span className="font-mono text-lg text-gray-900 dark:text-gray-100">
                    {tree.string}
                  </span>
                </div>
                <div className="flex gap-2 text-sm">
                  <span className="px-3 py-1 rounded bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300">
                    Depth: {tree.depth}
                  </span>
                  <span className="px-3 py-1 rounded bg-purple-100 dark:bg-purple-900 text-purple-700 dark:text-purple-300">
                    Width: {tree.width}
                  </span>
                  <span className="px-3 py-1 rounded bg-emerald-100 dark:bg-emerald-900 text-emerald-700 dark:text-emerald-300">
                    {tree.shape.replace('-', ' ')}
                  </span>
                </div>
              </div>
            </button>
          ))}
        </div>
      )}

      {/* Selected Tree Details */}
      {selectedTree && (
        <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">
              Tree #{selectedTree.index}
            </h2>
            <button
              onClick={() => setSelectedTree(null)}
              className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200"
            >
              ✕
            </button>
          </div>

          <div className="space-y-4">
            {/* Tree String */}
            <div>
              <p className="text-sm font-medium text-gray-600 dark:text-gray-400 mb-1">
                Parentheses Notation
              </p>
              <p className="font-mono text-2xl text-emerald-600 dark:text-emerald-400 p-3 bg-emerald-50 dark:bg-emerald-950 rounded">
                {selectedTree.string}
              </p>
            </div>

            {/* Properties Grid */}
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="p-3 rounded bg-blue-50 dark:bg-blue-950 border border-blue-200 dark:border-blue-800">
                <p className="text-xs text-blue-600 dark:text-blue-400 mb-1">Depth</p>
                <p className="text-2xl font-bold text-blue-700 dark:text-blue-300">
                  {selectedTree.depth}
                </p>
              </div>
              <div className="p-3 rounded bg-purple-50 dark:bg-purple-950 border border-purple-200 dark:border-purple-800">
                <p className="text-xs text-purple-600 dark:text-purple-400 mb-1">Width</p>
                <p className="text-2xl font-bold text-purple-700 dark:text-purple-300">
                  {selectedTree.width}
                </p>
              </div>
              <div className="p-3 rounded bg-teal-50 dark:bg-teal-950 border border-teal-200 dark:border-teal-800">
                <p className="text-xs text-teal-600 dark:text-teal-400 mb-1">Nodes</p>
                <p className="text-2xl font-bold text-teal-700 dark:text-teal-300">
                  {selectedTree.nodes}
                </p>
              </div>
              <div className="p-3 rounded bg-emerald-50 dark:bg-emerald-950 border border-emerald-200 dark:border-emerald-800">
                <p className="text-xs text-emerald-600 dark:text-emerald-400 mb-1">Index</p>
                <p className="text-2xl font-bold text-emerald-700 dark:text-emerald-300">
                  {selectedTree.index}
                </p>
              </div>
            </div>

            {/* Shape Classification */}
            <div className="p-4 rounded bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-700">
              <p className="text-sm font-medium text-gray-600 dark:text-gray-400 mb-1">
                Tree Shape
              </p>
              <p className="text-lg font-semibold text-gray-900 dark:text-gray-100">
                {selectedTree.shape.replace('-', ' ').split(' ').map(w => 
                  w.charAt(0).toUpperCase() + w.slice(1)
                ).join(' ')}
              </p>
            </div>

            {/* Pattern Family */}
            <div className="p-4 rounded bg-emerald-50 dark:bg-emerald-950 border border-emerald-200 dark:border-emerald-800">
              <p className="text-sm font-medium text-emerald-600 dark:text-emerald-400 mb-1">
                Pattern Dynamics Family
              </p>
              <p className="text-lg font-semibold text-emerald-900 dark:text-emerald-100">
                {selectedTree.patternFamily.replace('-', ' ').split(' ').map(w => 
                  w.charAt(0).toUpperCase() + w.slice(1)
                ).join(' ')}
              </p>
            </div>

            {/* Meaning */}
            <div className="p-4 rounded bg-blue-50 dark:bg-blue-950 border border-blue-200 dark:border-blue-800">
              <p className="text-sm font-medium text-blue-600 dark:text-blue-400 mb-2">
                Pattern Dynamics Meaning
              </p>
              <p className="text-gray-700 dark:text-gray-300 leading-relaxed">
                {selectedTree.meaning}
              </p>
            </div>
          </div>
        </div>
      )}

      {/* Shape Statistics */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-xl font-bold mb-4 text-gray-900 dark:text-gray-100">
          Distribution by Shape
        </h2>
        <div className="space-y-3">
          {mapping.statistics.map((stat) => (
            <div key={stat.shape} className="flex items-center justify-between">
              <div className="flex-1">
                <p className="font-medium text-gray-900 dark:text-gray-100">
                  {stat.shape.replace('-', ' ').split(' ').map(w => 
                    w.charAt(0).toUpperCase() + w.slice(1)
                  ).join(' ')}
                </p>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  {stat.patternFamily.replace('-', ' ').split(' ').map(w => 
                    w.charAt(0).toUpperCase() + w.slice(1)
                  ).join(' ')}
                </p>
              </div>
              <div className="text-right">
                <p className="text-2xl font-bold text-emerald-600 dark:text-emerald-400">
                  {stat.count}
                </p>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  {stat.percentage.toFixed(1)}%
                </p>
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Conceptual Correspondences */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-xl font-bold mb-4 text-gray-900 dark:text-gray-100">
          Tree-Pattern Correspondences
        </h2>
        <div className="space-y-3">
          {mapping.correspondences.map((corr, idx) => (
            <div key={idx} className="p-4 rounded bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-700">
              <div className="flex items-start gap-3">
                <div className="flex-shrink-0 w-8 h-8 rounded-full bg-emerald-600 text-white flex items-center justify-center font-bold">
                  {idx + 1}
                </div>
                <div className="flex-1">
                  <div className="flex items-center gap-2 mb-1">
                    <span className="font-semibold text-gray-900 dark:text-gray-100">
                      {corr.treeConcept}
                    </span>
                    <span className="text-gray-400">↔</span>
                    <span className="font-semibold text-emerald-600 dark:text-emerald-400">
                      {corr.patternMeaning}
                    </span>
                  </div>
                  <p className="text-sm text-gray-600 dark:text-gray-400">
                    {corr.explanation}
                  </p>
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
