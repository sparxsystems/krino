using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class MorphemeEqualityComparer : IEqualityComparer<IMorpheme>
    {
        public bool Equals(IMorpheme x, IMorpheme y)
        {
            if (x.Morph == y.Morph &&
                x.GrammarCharacter == y.GrammarCharacter &&
                x.Attributes == y.Attributes)
            {
                return true;
            }

            return false;
        }

        public int GetHashCode(IMorpheme obj)
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ obj.Morph.GetHashCode();
            hash = (hash * 16777619) ^ obj.GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ obj.Attributes.GetHashCode();

            return hash;
        }

        //private string SememesToString(IEnumerable<ISememe> sememes)
        //{
        //    IEnumerable<ISememe> leaves = sememes.SelectMany(x => x)
        //        .Where(x => x.Hyponymies == null || !x.Hyponymies.Any());

        //    IEnumerable<IEnumerable<ISememe>> pathsOfLeaves = leaves.Select(x => x.Parents.Concat(x))
        //        .Reverse()
        //        .OrderBy(x => x.First().Meaning)
        //        .ThenBy(x => x.First().Value);

        //    StringBuilder builder = new StringBuilder();

        //    foreach (IEnumerable<ISememe> sememePath in pathsOfLeaves)
        //    {
        //        builder.AppendLine(string.Join("; ", sememePath.Select(x => string.Join(":", x.Meaning, x.Value))));
        //    }

        //    return builder.ToString();
        //}
    }
}
