interface TargetChoice {
  tag: string
  target?: { tag: string; contents?: unknown }
}

// A question can offer a card that has left every visible zone (for example,
// an exiled card). Only expose cards the server actually offered as targets.
export function unanchoredCardTargets<Card>(
  choices: readonly TargetChoice[],
  cards: Readonly<Record<string, Card>>,
  renderedCardIds: ReadonlySet<string>,
): Card[] {
  const seen = new Set<string>()
  const result: Card[] = []
  for (const choice of choices) {
    const target = choice.target
    if (choice.tag !== 'TargetLabel' || target?.tag !== 'CardIdTarget') continue
    const id = target.contents
    if (typeof id !== 'string' || renderedCardIds.has(id) || seen.has(id)) continue
    const card = cards[id]
    if (card === undefined) continue
    seen.add(id)
    result.push(card)
  }
  return result
}
