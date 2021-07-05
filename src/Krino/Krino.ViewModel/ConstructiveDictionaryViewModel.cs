using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.ViewModel
{
    public class ConstructiveDictionaryViewModel : IConstructiveDictionaryViewModel
    {
        private IConstructiveDictionary myConstructiveDictionary;
        private AdTreeCreator myAdTreeCreator;

        public ConstructiveDictionaryViewModel()
        {
            myConstructiveDictionary = new EnglishConstructiveDictionaryFactory().Create();
            myAdTreeCreator = new AdTreeCreator(myConstructiveDictionary);
        }

        public IEnumerable<Morpheme> Lexemes => myConstructiveDictionary.Lexemes;

        public IEnumerable<Morpheme> NonLexemes => myConstructiveDictionary.NonLexemes;

        public IEnumerable<Pattern> Patterns => myConstructiveDictionary.Patterns;

        public IReadOnlyList<IAdTree> GetAdTree(string phrase)
        {
            var phraseToUse = phrase.ToLower().Split(" ", StringSplitOptions.RemoveEmptyEntries);
            var result = myAdTreeCreator.Create(phraseToUse);
            return result;
        }

        public string GetAdTreeVisualization(IAdTree adTree)
        {
            StringBuilder result = new StringBuilder();

            var rows = GetRows(adTree).ToList();
            foreach (var row in rows)
            {
                result.AppendLine(row);
            }

            return result.ToString();
        }

        private IEnumerable<string> GetRows(IAdTree adTree)
        {
            var morphemeAdTrees = adTree.GetAdTreesInAdTreeOrder().ToList();

            var width = morphemeAdTrees.Select(x => GetAdTreeItemVisualization(x).Length).Sum() + 10;
            var height = morphemeAdTrees.Max(x => x.AdPositions.Count() + 1);
            var matrix = new char[height][];
            for (int i = 0; i < matrix.Length; ++i)
            {
                matrix[i] = new string(' ', width).ToCharArray();
            }

            Dictionary<IAdTree, Tuple<int, int, int>> matrixPositions = new Dictionary<IAdTree, Tuple<int, int, int>>();

            int currentIdx = width - 5;
            for (int i = 0; i < morphemeAdTrees.Count; ++i)
            {
                var adTreeItem = morphemeAdTrees[i];
                var toPut = GetAdTreeItemVisualization(adTreeItem);
                var row = adTreeItem.AdPositions.Count();
                var column = currentIdx - toPut.Length;

                matrixPositions[adTreeItem] = Tuple.Create(row, column, toPut.Length);

                // Put it into the matrix.
                for (int j = 0; j < toPut.Length; ++j)
                {
                    var ch = toPut[j];
                    matrix[row][column + j] = ch;
                }

                currentIdx -= toPut.Length;
            }

            // Draw lines.
            for (int i = 0; i < morphemeAdTrees.Count; ++i)
            {
                var adTreeItem = morphemeAdTrees[i];
                matrixPositions.TryGetValue(adTreeItem, out var adTreeItemPosition);
                if (adTreeItemPosition != null)
                {
                    if (adTreeItem.Left != null)
                    {
                        matrixPositions.TryGetValue(adTreeItem.Left, out var leftChildPosition);
                        if (leftChildPosition != null)
                        {
                            var startPos = leftChildPosition.Item2 + leftChildPosition.Item3 / 2;
                            var endPos = adTreeItemPosition.Item2;
                            for (int j = startPos; j < endPos; ++j)
                            {
                                matrix[adTreeItemPosition.Item1][j] = '─';
                            }
                        }
                    }

                    if (adTreeItem.Right != null)
                    {
                        matrixPositions.TryGetValue(adTreeItem.Right, out var rightChildPosition);
                        if (rightChildPosition != null)
                        {
                            var startPos = adTreeItemPosition.Item2 + adTreeItemPosition.Item3;
                            var endPos = rightChildPosition.Item2 + rightChildPosition.Item3 / 2;
                            for (int j = startPos; j < endPos; ++j)
                            {
                                matrix[adTreeItemPosition.Item1][j] = '─';
                            }
                        }
                    }
                }
            }


            foreach (var rowStr in matrix)
            {
                yield return new string(rowStr);
            }
        }

        private string GetAdTreeItemVisualization(IAdTree adTree)
        {
            var grammarCharacter = adTree.Morpheme != null ? $"[{adTree.Morpheme.GrammarCharacter}]" : "";
            var pattern = !adTree.Pattern.IsMorpheme ? $"{{{adTree.Pattern.Name}}}" : "";
            var morpheme = !string.IsNullOrEmpty(adTree.Morpheme?.Morph) ? $"{adTree.Morpheme?.Morph}" : "";

            var result = new StringBuilder();

            if (pattern != "")
            {
                result.Append(pattern);
            }
            if (grammarCharacter != "")
            {
                result.Append(grammarCharacter);
            }
            if (morpheme != "")
            {
                result.Append(morpheme);
            }

            return result.ToString();
        }
    }
}
