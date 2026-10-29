import type { Game } from '@/arkham/types/Game'

interface TargetChoice {
  tag: string
  target?: { tag: string; contents?: unknown }
}

export function unanchoredCardTargetIds(
  choices: readonly TargetChoice[], renderedCardIds: ReadonlySet<string>,
): Set<string> {
  const ids = new Set<string>()
  for (const choice of choices) {
    const target = choice.target
    if (choice.tag !== 'TargetLabel' || target?.tag !== 'CardIdTarget') continue
    if (typeof target.contents === 'string' && !renderedCardIds.has(target.contents)) ids.add(target.contents)
  }
  return ids
}

// The outer modal and its inner question must agree about which cards already
// have a clickable home. A card existing in game.cards is not itself visible.
export function visibleCardChoiceIds(game: Game, playerId: string): Set<string> {
  const ids = new Set<string>()
  const addCard = (card: unknown): void => {
    if (!card || typeof card !== 'object') return
    if ('id' in card && typeof card.id === 'string') ids.add(card.id)
    else if ('contents' in card) addCard(card.contents)
  }
  const add = (cards: readonly unknown[] | undefined) => cards?.forEach(addCard)
  const investigators = Object.values(game.investigators ?? {})
  const investigator = investigators.find(i => i.playerId === playerId)
  add(investigator?.hand)
  for (const i of investigators) { add(i.discard); add(i.cardsUnderneath) }
  add(game.focusedCards)
  add(game.removedFromPlay)
  for (const groups of [game.foundCards, game.scenario?.foundCards, investigator?.foundCards]) {
    for (const cards of Object.values(groups ?? {})) add(cards)
  }
  for (const entities of [game.assets, game.events, game.locations, game.acts]) {
    for (const entity of Object.values(entities ?? {})) add(entity.cardsUnderneath)
  }
  const scenario = game.scenario
  if (scenario) {
    for (const cards of [scenario.discard, scenario.victoryDisplay, scenario.setAsideCards,
      scenario.cardsUnderScenarioReference, scenario.cardsUnderAgendaDeck, scenario.cardsUnderActDeck,
      scenario.cardsNextToAgendaDeck, scenario.cardsNextToActDeck]) add(cards)
    for (const [, cards] of scenario.deckDiscards ?? []) add(cards)
  }
  add(game.skillTest?.committedCards)
  return ids
}

// A question can offer a card that has left every visible zone (for example,
// an exiled card). Only expose cards the server actually offered as targets.
export function unanchoredCardTargets<Card>(
  choices: readonly TargetChoice[],
  cards: Readonly<Record<string, Card>>,
  renderedCardIds: ReadonlySet<string>,
): Card[] {
  const result: Card[] = []
  for (const id of unanchoredCardTargetIds(choices, renderedCardIds)) {
    const card = cards[id]
    if (card === undefined) continue
    result.push(card)
  }
  return result
}
