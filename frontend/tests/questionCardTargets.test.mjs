import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import ts from 'typescript'

const source = await readFile(new URL('../src/arkham/questionCardTargets.ts', import.meta.url), 'utf8')
const { outputText } = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.ESNext, target: ts.ScriptTarget.ES2022 },
})
const { unanchoredCardTargets } = await import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
const target = (id) => ({ tag: 'TargetLabel', target: { tag: 'CardIdTarget', contents: id } })

test('Put out the Fire can display an exiled card with no focused or board representation', () => {
  const id = 'fceb89f1-82a1-43ee-8537-4b8e5efbbc6b'
  const exiled = { tag: 'PlayerCard', contents: { id, cardCode: '01088' } }
  assert.deepEqual(unanchoredCardTargets([target(id)], { [id]: exiled }, new Set()), [exiled])
})

test('cards already visible in hand, discard, search or focused cards are not repeated', () => {
  const card = { id: 'visible' }
  assert.deepEqual(unanchoredCardTargets([target('visible')], { visible: card }, new Set(['visible'])), [])
})

test('only offered card IDs are exposed, once each', () => {
  const offered = { id: 'offered' }
  const hidden = { id: 'hidden' }
  assert.deepEqual(unanchoredCardTargets(
    [target('offered'), target('offered'), target('missing'), { tag: 'TargetLabel', target: { tag: 'EnemyTarget', contents: 'hidden' } }],
    { offered, hidden }, new Set(),
  ), [offered])
})
