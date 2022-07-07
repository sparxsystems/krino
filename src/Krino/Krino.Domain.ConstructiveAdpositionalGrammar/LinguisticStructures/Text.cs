using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    internal class Text : LinguisticStructureBase, IText
    {
        public Text(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<ISentence> Sentences { get; } = new List<ISentence>();

        public string Value => string.Join(" ", Sentences.Select(x => x.Value)).Trim();

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", Sentences.Select(x => x.GrammarStr)), ")");

        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);
            Sentences.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
        }

        public ILinguisticStructure DeepCopy()
        {
            var sentences = Sentences.Select(x => x.DeepCopy()).OfType<ISentence>();
            var result = new Text(Attributes);
            result.Sentences.AddRange(sentences);
            return result;
        }
    }
}
