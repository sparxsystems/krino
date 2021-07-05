using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class AdTreeFactory : IEnumerable<AdTreeFactory>
    {
        public AdTreeFactory(Pattern patter)
        {
            Pattern = patter;
        }

        public Pattern Pattern { get; private set; }
        public AdTreeFactory Right { get; set; }
        public AdTreeFactory Left { get; set; }

        public string PatternSignature
        {
            get
            {
                var result = new StringBuilder();

                foreach (var item in this)
                {
                    if (item.Pattern.IsLikeMorpheme)
                    {
                        result.Append(item.Pattern.UpRule.GrammarCharacter);
                    }
                    else
                    {
                        if (item.Pattern.IsLeftFirst)
                        {
                            if (item.Left == null && item.Pattern.LeftRule.GrammarCharacter != GrammarCharacter.e)
                            {
                                result.Append(item.Pattern.LeftRule.GrammarCharacter);
                            }
                            if (item.Pattern.IsMorphematicAdPosition())
                            {
                                result.Append(item.Pattern.UpRule.GrammarCharacter);
                            }
                            if (item.Right == null && item.Pattern.LeftRule.GrammarCharacter != GrammarCharacter.e)
                            {
                                result.Append(item.Pattern.RightRule.GrammarCharacter);
                            }
                        }
                        else
                        {
                            if (item.Right == null && item.Pattern.LeftRule.GrammarCharacter != GrammarCharacter.e)
                            {
                                result.Append(item.Pattern.RightRule.GrammarCharacter);
                            }
                            if (item.Pattern.IsMorphematicAdPosition())
                            {
                                result.Append(item.Pattern.UpRule.GrammarCharacter);
                            }
                            if (item.Left == null && item.Pattern.LeftRule.GrammarCharacter != GrammarCharacter.e)
                            {
                                result.Append(item.Pattern.LeftRule.GrammarCharacter);
                            }
                        }
                    }
                }

                return result.ToString();
            }
        }

        public IAdTree CreateAdTree(IAttributesModel attributesModel, IEnumerable<IAdTree> morphemes)
        {
            var result = CreateAdTreeIntern(attributesModel, new Stack<IAdTree>(morphemes.Reverse()));
            return result;
        }


        private IAdTree CreateAdTreeIntern(IAttributesModel attributesModel, Stack<IAdTree> morphemeAdTrees)
        {
            IAdTree thisAdTree;
            IAdTree leftAdTree;
            IAdTree rightAdTree;

            if (Pattern.IsLeftFirst)
            {
                leftAdTree = Left != null ? Left.CreateAdTreeIntern(attributesModel, morphemeAdTrees) : TryPop(morphemeAdTrees);


                if (Pattern.IsMorphematicAdPosition())
                {
                    var adPositionMorphemeAdTree = TryPop(morphemeAdTrees);
                    thisAdTree = new AdTree(adPositionMorphemeAdTree.Morpheme, Pattern);
                }
                else
                {
                    thisAdTree = new AdTree(Morpheme.Epsilon(attributesModel), Pattern);
                }

                rightAdTree = Right != null ? Right.CreateAdTreeIntern(attributesModel, morphemeAdTrees) : TryPop(morphemeAdTrees);
            }
            else
            {
                rightAdTree = Right != null ? Right.CreateAdTreeIntern(attributesModel, morphemeAdTrees) : TryPop(morphemeAdTrees);

                if (Pattern.IsMorphematicAdPosition())
                {
                    var adPositionMorphemeAdTree = TryPop(morphemeAdTrees);
                    var adPositionMorpheme = adPositionMorphemeAdTree?.Morpheme ?? Morpheme.Epsilon(attributesModel);
                    thisAdTree = new AdTree(adPositionMorpheme, Pattern);
                }
                else
                {
                    thisAdTree = new AdTree(Morpheme.Epsilon(attributesModel), Pattern);
                }

                leftAdTree = Left != null ? Left.CreateAdTreeIntern(attributesModel, morphemeAdTrees) : TryPop(morphemeAdTrees);
            }

            thisAdTree.Left = leftAdTree;
            thisAdTree.Right = rightAdTree;

            return thisAdTree;
        }

        private IAdTree TryPop(Stack<IAdTree> stack) => stack.Count > 0 ? stack.Pop() : null;

        public IEnumerator<AdTreeFactory> GetEnumerator()
        {
            // Note: using the stack has a better performance than a recursive call.
            //       Also there is not a danger of the stack overflow.
            // Note: the enumeration order matches the adtree order.
            var stack = new Stack<AdTreeFactory>();
            stack.Push(this);

            var adPositionStack = new Stack<AdTreeFactory>();

            while (stack.Count > 0)
            {
                var aThis = stack.Pop();

                if (aThis != null && (aThis.Left != null || aThis.Right != null))
                {
                    adPositionStack.Push(aThis);

                    // If left is before right.
                    if (aThis.Pattern.IsLeftFirst)
                    {
                        stack.Push(aThis.Right);
                        stack.Push(aThis.Left);
                    }
                    else
                    {
                        stack.Push(aThis.Left);
                        stack.Push(aThis.Right);
                    }
                }
                else
                {
                    if (aThis != null)
                    {
                        yield return aThis;
                    }

                    if (adPositionStack.Count > 0)
                    {
                        var adPosition = adPositionStack.Pop();
                        yield return adPosition;
                    }
                }
            }
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
