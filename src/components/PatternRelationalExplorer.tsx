/**
 * PatternRelationalExplorer.tsx
 * Interactive visualization of Pattern Dynamics relational expressions
 */

import React, { useState } from 'react';
import {
  CORE_PATTERNS,
  getPatternAspect,
  evaluateRelation,
  type CorePattern,
  type SetOperator
} from '../lib/pattern-relational.types';
import {
  PATTERN_MATRIX,
  GLOBAL_PROCESSES,
  META_PATTERNS,
  HOLARCHICAL_RELATIONS
} from '../lib/pattern-relational.data';

export function PatternRelationalExplorer() {
  const [selectedFrom, setSelectedFrom] = useState<CorePattern>('source');
  const [selectedTo, setSelectedTo] = useState<CorePattern>('dynamics');
  const [selectedOperator, setSelectedOperator] = useState<SetOperator>('⊂');

  const relationship = PATTERN_MATRIX[selectedFrom][selectedTo];
  const aspect = getPatternAspect(selectedFrom, selectedTo);
  const evaluation = evaluateRelation(selectedFrom, selectedOperator, selectedTo);

  const globalProcesses = GLOBAL_PROCESSES.filter(gp => 
    gp.examples.some(ex => 
      ex.toLowerCase().includes(selectedFrom) || 
      ex.toLowerCase().includes(selectedTo)
    )
  );

  const holarchies = HOLARCHICAL_RELATIONS.filter(hr =>
    (hr.parent === selectedFrom && hr.child === selectedTo) ||
    (hr.child === selectedFrom && hr.parent === selectedTo)
  );

  return (
    <div className="w-full max-w-6xl mx-auto p-6 space-y-6">
      <div className="bg-gradient-to-r from-blue-50 to-purple-50 dark:from-blue-950 dark:to-purple-950 rounded-lg p-6 border border-blue-200 dark:border-blue-800">
        <h1 className="text-3xl font-bold mb-2 text-blue-900 dark:text-blue-100">
          Pattern Dynamics Relational Explorer
        </h1>
        <p className="text-blue-700 dark:text-blue-300">
          Explore the 7×7 matrix of 49 pattern relationships and holarchical structures
        </p>
      </div>

      {/* Pattern Selectors */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
            From Pattern
          </label>
          <select
            value={selectedFrom}
            onChange={(e) => setSelectedFrom(e.target.value as CorePattern)}
            className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
          >
            {CORE_PATTERNS.map(pattern => (
              <option key={pattern} value={pattern}>
                {pattern.charAt(0).toUpperCase() + pattern.slice(1)}
              </option>
            ))}
          </select>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
            Operator
          </label>
          <select
            value={selectedOperator}
            onChange={(e) => setSelectedOperator(e.target.value as SetOperator)}
            className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
          >
            <option value="∈">∈ (IS)</option>
            <option value="⊂">⊂ (Dynamics)</option>
            <option value="∩">∩ (OR)</option>
            <option value="⊆">⊆ (Exchange)</option>
            <option value="∪">∪ (Structure)</option>
            <option value="∉">∉ (NOT)</option>
            <option value="∅">∅ (EMPTY)</option>
          </select>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-lg p-4 border border-gray-200 dark:border-gray-700">
          <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
            To Pattern
          </label>
          <select
            value={selectedTo}
            onChange={(e) => setSelectedTo(e.target.value as CorePattern)}
            className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
          >
            {CORE_PATTERNS.map(pattern => (
              <option key={pattern} value={pattern}>
                {pattern.charAt(0).toUpperCase() + pattern.slice(1)}
              </option>
            ))}
          </select>
        </div>
      </div>

      {/* Relationship Display */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-xl font-semibold mb-4 text-gray-900 dark:text-gray-100">
          Relationship Details
        </h2>
        
        <div className="space-y-4">
          <div>
            <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
              Relationship Name:
            </span>
            <p className="text-lg font-semibold text-blue-600 dark:text-blue-400">
              {relationship.relationship}
            </p>
          </div>

          <div>
            <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
              Description:
            </span>
            <p className="text-gray-700 dark:text-gray-300">
              {relationship.description}
            </p>
          </div>

          {relationship.cellNumber && (
            <div>
              <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
                Cell Number:
              </span>
              <p className="text-lg font-semibold text-purple-600 dark:text-purple-400">
                #{relationship.cellNumber}
              </p>
            </div>
          )}

          {aspect && (
            <div>
              <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
                Aspect Quality:
              </span>
              <p className="text-gray-700 dark:text-gray-300">
                {aspect.quality}
              </p>
            </div>
          )}

          <div className="pt-4 border-t border-gray-200 dark:border-gray-700">
            <span className="text-sm font-medium text-gray-500 dark:text-gray-400">
              Relational Expression:
            </span>
            <p className="text-lg font-mono text-gray-900 dark:text-gray-100 bg-gray-100 dark:bg-gray-900 p-2 rounded">
              {evaluation}
            </p>
          </div>
        </div>
      </div>

      {/* Global Processes */}
      {globalProcesses.length > 0 && (
        <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
          <h2 className="text-xl font-semibold mb-4 text-gray-900 dark:text-gray-100">
            Related Global Processes
          </h2>
          <div className="space-y-3">
            {globalProcesses.map((gp, idx) => (
              <div key={idx} className="bg-gray-50 dark:bg-gray-900 p-3 rounded border border-gray-200 dark:border-gray-700">
                <div className="flex items-center gap-2 mb-1">
                  <span className="font-semibold text-purple-600 dark:text-purple-400">
                    Process #{gp.number}
                  </span>
                  <span className="text-sm text-gray-600 dark:text-gray-400">
                    ({gp.type} - {gp.scope})
                  </span>
                </div>
                <p className="text-sm text-gray-700 dark:text-gray-300">
                  {gp.mechanism}
                </p>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Holarchical Relations */}
      {holarchies.length > 0 && (
        <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
          <h2 className="text-xl font-semibold mb-4 text-gray-900 dark:text-gray-100">
            Holarchical Relations
          </h2>
          <div className="space-y-3">
            {holarchies.map((hr, idx) => (
              <div key={idx} className="bg-gray-50 dark:bg-gray-900 p-3 rounded border border-gray-200 dark:border-gray-700">
                <div className="flex items-center gap-2 mb-1">
                  <span className="font-semibold text-blue-600 dark:text-blue-400">
                    {String(hr.parent)} {hr.type} {String(hr.child)}
                  </span>
                  <span className="text-sm text-gray-600 dark:text-gray-400">
                    (Level {hr.level})
                  </span>
                  {hr.bidirectional && (
                    <span className="text-xs bg-green-100 dark:bg-green-900 text-green-700 dark:text-green-300 px-2 py-1 rounded">
                      Bidirectional
                    </span>
                  )}
                </div>
                <p className="text-sm text-gray-700 dark:text-gray-300">
                  {hr.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Meta-Patterns */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-xl font-semibold mb-4 text-gray-900 dark:text-gray-100">
          Meta-Patterns (Second Order)
        </h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {META_PATTERNS.map((mp, idx) => (
            <div key={idx} className="bg-gradient-to-br from-purple-50 to-blue-50 dark:from-purple-950 dark:to-blue-950 p-4 rounded border border-purple-200 dark:border-purple-800">
              <h3 className="font-semibold text-purple-900 dark:text-purple-100 mb-1">
                {mp.name}
              </h3>
              <p className="text-sm text-purple-700 dark:text-purple-300 mb-2">
                {mp.description}
              </p>
              <div className="text-xs text-purple-600 dark:text-purple-400">
                <span className="font-medium">Composed from: </span>
                {mp.composedFrom.join(', ')}
              </div>
              <div className="text-xs text-purple-600 dark:text-purple-400 mt-1">
                <span className="font-medium">Emergent: </span>
                {mp.emergentProperty}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Pattern Matrix Visualization */}
      <div className="bg-white dark:bg-gray-800 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-xl font-semibold mb-4 text-gray-900 dark:text-gray-100">
          7×7 Pattern Matrix
        </h2>
        <div className="overflow-x-auto">
          <table className="w-full border-collapse text-sm">
            <thead>
              <tr>
                <th className="border border-gray-300 dark:border-gray-600 p-2 bg-gray-100 dark:bg-gray-900"></th>
                {CORE_PATTERNS.map(pattern => (
                  <th key={pattern} className="border border-gray-300 dark:border-gray-600 p-2 bg-gray-100 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
                    {pattern.charAt(0).toUpperCase() + pattern.slice(1)}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {CORE_PATTERNS.map(fromPattern => (
                <tr key={fromPattern}>
                  <th className="border border-gray-300 dark:border-gray-600 p-2 bg-gray-100 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
                    {fromPattern.charAt(0).toUpperCase() + fromPattern.slice(1)}
                  </th>
                  {CORE_PATTERNS.map(toPattern => {
                    const rel = PATTERN_MATRIX[fromPattern][toPattern];
                    const isSelected = fromPattern === selectedFrom && toPattern === selectedTo;
                    return (
                      <td
                        key={toPattern}
                        className={`border border-gray-300 dark:border-gray-600 p-2 cursor-pointer transition-colors ${
                          isSelected
                            ? 'bg-blue-200 dark:bg-blue-900'
                            : 'hover:bg-gray-100 dark:hover:bg-gray-700'
                        }`}
                        onClick={() => {
                          setSelectedFrom(fromPattern);
                          setSelectedTo(toPattern);
                        }}
                        title={rel.relationship}
                      >
                        <div className="text-xs text-gray-700 dark:text-gray-300">
                          {rel.cellNumber ? `#${rel.cellNumber}` : '•'}
                        </div>
                      </td>
                    );
                  })}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        <p className="text-sm text-gray-600 dark:text-gray-400 mt-2">
          Click any cell to explore that relationship
        </p>
      </div>
    </div>
  );
}
