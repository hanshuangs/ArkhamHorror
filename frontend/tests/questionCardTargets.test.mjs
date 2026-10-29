import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import ts from 'typescript'

const source = await readFile(new URL('../src/arkham/questionCardTargets.ts', import.meta.url), 'utf8')
const { outputText } = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.ESNext, target: ts.ScriptTarget.ES2022 },
})
const { unanchoredCardTargets, unanchoredCardTargetIds, visibleCardChoiceIds } = await import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
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

test('the outer modal opens for a saved Put out the Fire choice with no focused cards', () => {
  const id = 'fceb89f1-82a1-43ee-8537-4b8e5efbbc6b'
  const exiled = { tag: 'PlayerCard', contents: { id, cardCode: 'c60505' } }
  const game = {
    cards: { [id]: exiled }, focusedCards: [], removedFromPlay: [],
    investigators: { robert: { playerId: 'player', hand: [], discard: [], cardsUnderneath: [], meta: { exiledCards: [exiled] } } },
  }
  const anchors = visibleCardChoiceIds(game, 'player')
  assert.equal(unanchoredCardTargetIds([target(id)], anchors).size, 1)
  assert.deepEqual(unanchoredCardTargets([target(id)], game.cards, anchors), [exiled])
})

test('visible zone cards keep their existing click targets without opening an extra modal', () => {
  const card = id => ({ tag: 'PlayerCard', contents: { id } })
  const game = {
    focusedCards: [card('focused')], foundCards: { FromDeck: [card('searched')] },
    investigators: { self: { playerId: 'player', hand: [card('hand')], discard: [{ id: 'discard' }], cardsUnderneath: [] } },
    skillTest: { committedCards: [card('committed')] },
  }
  const anchors = visibleCardChoiceIds(game, 'player')
  assert.equal(unanchoredCardTargetIds(['hand', 'discard', 'focused', 'searched', 'committed'].map(target), anchors).size, 0)
})

test('an unknown offered card still opens the outer modal for the generic choice fallback', () => {
  assert.equal(unanchoredCardTargetIds([target('unknown')], new Set()).size, 1)
  assert.deepEqual(unanchoredCardTargets([target('unknown')], {}, new Set()), [])
})
