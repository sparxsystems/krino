using Krino.Vertical.Utils.Strings;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    internal class Clause : LinguisticStructureBase, IClause
    {
        public Clause(BigInteger attributes)
            : base(attributes)
        {
        }

        public IWord Conjunction { get; set; }
        public ISubject Subject { get; set; }
        public IPredicate Predicate { get; set; }

        public IEnumerable<IClause> DependentClauses
        {
            get
            {
                var clausesInSubject = Subject.AllItems.OfType<IClause>();
                var clausesInPredicate = Predicate.AllItems.OfType<IClause>();

                var result = clausesInSubject.Concat(clausesInPredicate);
                return result;
            }
        }

        public IClause IndependentClause
        {
            get
            {
                var result = DeepCopy() as IClause;

                while (true)
                {
                    var phrases = result.Subject.AllItems.OfType<IPhrase>().Concat(result.Predicate.AllItems.OfType<IPhrase>());
                    var phraseWithClause = phrases.FirstOrDefault(x => x.DirectItems.OfType<IClause>().Any());
                    
                    if (phraseWithClause != null)
                    {
                        phraseWithClause.DirectItems.RemoveAll(x => x is IClause);
                    }
                    else
                    {
                        break;
                    }
                }

                return result;
            }
        }

        public string Value => StringExt.JoinIgnoreEmpty(" ", Conjunction?.Value, Subject?.Value, Predicate?.Value).Trim();

        public string GrammarStr => string.Join("", AttributesStr, "(", StringExt.JoinIgnoreEmpty(" ", AttributesStr, Conjunction?.GrammarStr, Subject?.GrammarStr, Predicate?.GrammarStr), ")");

        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);

            Conjunction?.BuildFormattedGrammarStr(indent + 4, builder);
            Subject?.BuildFormattedGrammarStr(indent + 4, builder);
            Predicate?.BuildFormattedGrammarStr(indent + 4, builder);
        }

        public ILinguisticStructure DeepCopy()
        {
            var result = new Clause(Attributes);
            result.Conjunction = Conjunction?.DeepCopy() as IWord;
            result.Subject = Subject?.DeepCopy() as ISubject;
            result.Predicate = Predicate?.DeepCopy() as IPredicate;
            return result;
        }
    }
}
