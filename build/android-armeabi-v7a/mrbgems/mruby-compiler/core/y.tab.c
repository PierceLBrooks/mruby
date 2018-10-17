
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 7 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"

#undef PARSER_DEBUG
#ifdef PARSER_DEBUG
# define YYDEBUG 1
#endif
#define YYERROR_VERBOSE 1
/*
 * Force yacc to use our memory management.  This is a little evil because
 * the macros assume that "parser_state *p" is in scope
 */
#define YYMALLOC(n)    mrb_malloc(p->mrb, (n))
#define YYFREE(o)      mrb_free(p->mrb, (o))
#define YYSTACK_USE_ALLOCA 0

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include "node.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void void_expr_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n) ((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)     ((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)  ((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)   ((stack)&1)

#define COND_PUSH(n)    BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()      BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()   BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()        BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)  BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()    BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP() BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()      BITSTACK_SET_P(p->cmdarg_stack)

#define SET_LINENO(c,n) ((c)->lineno = (n))
#define NODE_LINENO(c,n) do {\
  if (n) {\
     (c)->filename_index = (n)->filename_index;\
     (c)->lineno = (n)->lineno;\
  }\
} while (0)

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))
#define nint(x) ((node*)(intptr_t)(x))
#define intn(x) ((int)(intptr_t)(x))

static inline mrb_sym
intern_cstr_gen(parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern_cstr(s) intern_cstr_gen(p,(s))

static inline mrb_sym
intern_gen(parser_state *p, const char *s, size_t len)
{
  return mrb_intern(p->mrb, s, len);
}
#define intern(s,len) intern_gen(p,(s),(len))

static inline mrb_sym
intern_gen_c(parser_state *p, const char c)
{
  return mrb_intern(p->mrb, &c, 1);
}
#define intern_c(c) intern_gen_c(p,(c))

static void
cons_free_gen(parser_state *p, node *cons)
{
  cons->cdr = p->cells;
  p->cells = cons;
}
#define cons_free(c) cons_free_gen(p, (c))

static void*
parser_palloc(parser_state *p, size_t size)
{
  void *m = mrb_pool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->jmp);
  }
  return m;
}

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node *)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  return c;
}
#define cons(a,b) cons_gen(p,(a),(b))

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b,0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c,0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
list4_gen(parser_state *p, node *a, node *b, node *c, node *d)
{
  return cons(a, cons(b, cons(c, cons(d, 0))));
}
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))

static node*
list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, 0)))));
}
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))

static node*
list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, 0))))));
}
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))

static node*
append_gen(parser_state *p, node *a, node *b)
{
  node *c = a;

  if (!a) return b;
  while (c->cdr) {
    c = c->cdr;
  }
  if (b) {
    c->cdr = b;
  }
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = (char *)parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#undef strndup
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

/* xxx ----------------------------- */

static node*
local_switch(parser_state *p)
{
  node *prev = p->locals;

  p->locals = cons(0, 0);
  return prev;
}

static void
local_resume(parser_state *p, node *prev)
{
  p->locals = prev;
}

static void
local_nest(parser_state *p)
{
  p->locals = cons(0, p->locals);
}

static void
local_unnest(parser_state *p)
{
  if (p->locals) {
    p->locals = p->locals->cdr;
  }
}

static mrb_bool
local_var_p(parser_state *p, mrb_sym sym)
{
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }
  return FALSE;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  if (p->locals) {
    p->locals->car = push(p->locals->car, nsym(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

static node*
locals_node(parser_state *p)
{
  return p->locals ? p->locals->car : NULL;
}

/* (:scope (vars..) (prog...)) */
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(locals_node(p), body));
}

/* (:begin prog...) */
static node*
new_begin(parser_state *p, node *body)
{
  if (body) {
    return list2((node*)NODE_BEGIN, body);
  }
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

/* (:rescue body rescue else) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

static node*
new_mod_rescue(parser_state *p, node *body, node *resq)
{
  return new_rescue(p, body, list1(list3(0, 0, resq)), 0);
}

/* (:ensure body ensure) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

/* (:nil) */
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

/* (:true) */
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

/* (:false) */
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

/* (:alias new old) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons(nsym(a), nsym(b)));
}

/* (:if cond then else) */
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, b, c);
}

/* (:unless cond then else) */
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, c, b);
}

/* (:while cond body) */
static node*
new_while(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_WHILE, cons(a, b));
}

/* (:until cond body) */
static node*
new_until(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_UNTIL, cons(a, b));
}

/* (:for var obj body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  void_expr_error(p, o);
  return list4((node*)NODE_FOR, v, o, b);
}

/* (:case a ((when ...) body) ((when...) body)) */
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  void_expr_error(p, a);
  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

/* (:postexe a) */
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

/* (:self) */
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

/* (:call a b c) */
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c, int pass)
{
  node *n = list4(nint(pass?NODE_CALL:NODE_SCALL), a, nsym(b), c);
  void_expr_error(p, a);
  NODE_LINENO(n, a);
  return n;
}

/* (:fcall self mid args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  node *n = new_self(p);
  NODE_LINENO(n, c);
  n = list4((node*)NODE_FCALL, n, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (:super . c) */
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

/* (:zsuper) */
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c) {
    if (c->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    return cons((node*)NODE_YIELD, c->car);
  }
  return cons((node*)NODE_YIELD, 0);
}

/* (:return . c) */
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

/* (:break . c) */
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

/* (:next . c) */
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

/* (:redo) */
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

/* (:retry) */
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

/* (:dot2 a b) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

/* (:dot3 a b) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

/* (:colon2 b c) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  void_expr_error(p, b);
  return cons((node*)NODE_COLON2, cons(b, nsym(c)));
}

/* (:colon3 . c) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, nsym(c));
}

/* (:and a b) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

/* (:array a...) */
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

/* (:splat . a) */
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

/* (:hash (k . v) (k . v)...) */
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

/* (:kw_hash (k . v) (k . v)...) */
static node*
new_kw_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_KW_HASH, a);
}

/* (:sym . a) */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, nsym(sym));
}

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  const char *s = (const char*)str->cdr->car;
  size_t len = (size_t)str->cdr->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:lvar . a) */
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, nsym(sym));
}

/* (:gvar . a) */
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, nsym(sym));
}

/* (:ivar . a) */
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, nsym(sym));
}

/* (:cvar . a) */
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, nsym(sym));
}

/* (:const . a) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, nsym(sym));
}

/* (:undef a...) */
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return list2((node*)NODE_UNDEF, nsym(sym));
}

/* (:class class super body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  void_expr_error(p, s);
  return list4((node*)NODE_CLASS, c, s, cons(locals_node(p), b));
}

/* (:sclass obj body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  void_expr_error(p, o);
  return list3((node*)NODE_SCLASS, o, cons(locals_node(p), b));
}

/* (:module module body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(locals_node(p), b));
}

/* (:def m lv (arg . body)) */
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, nsym(m), locals_node(p), a, b);
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  void_expr_error(p, o);
  return list6((node*)NODE_SDEF, o, nsym(m), locals_node(p), a, b);
}

/* (:arg . sym) */
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, nsym(sym));
}

/* (m o r m2 tail) */
/* m: (a b c) */
/* o: ((a . e1) (b . e2)) */
/* r: a */
/* m2: (a b c) */
/* b: a */
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, node *tail)
{
  node *n;

  n = cons(m2, tail);
  n = cons(nsym(rest), n);
  n = cons(opt, n);
  return cons(m, n);
}

/* (:args_tail keywords rest_keywords_sym block_sym) */
static node*
new_args_tail(parser_state *p, node *kws, node *kwrest, mrb_sym blk)
{
  node *k;

  /* allocate register for keywords hash */
  if (kws || kwrest) {
    local_add_f(p, (kwrest && kwrest->cdr)? sym(kwrest->cdr) :  mrb_intern_lit(p->mrb, "**"));
  }

  /* allocate register for block */
  local_add_f(p, blk? blk : mrb_intern_lit(p->mrb, "&"));

  // allocate register for keywords arguments
  // order is for Proc#parameters
  for (k = kws; k; k = k->cdr) {
    if (!k->car->cdr->cdr->car) { // allocate required keywords
      local_add_f(p, sym(k->car->cdr->car));
    }
  }
  for (k = kws; k; k = k->cdr) {
    if (k->car->cdr->cdr->car) { // allocate keywords with default
      local_add_f(p, sym(k->car->cdr->car));
    }
  }

  return list4((node*)NODE_ARGS_TAIL, kws, kwrest, nsym(blk));
}

/* (:kw_arg kw_sym def_arg) */
static node*
new_kw_arg(parser_state *p, mrb_sym kw, node *def_arg)
{
  mrb_assert(kw);
  return list3((node*)NODE_KW_ARG, nsym(kw), def_arg);
}

/* (:block_arg . a) */
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

/* (:block arg body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_BLOCK, locals_node(p), a, b);
}

/* (:lambda arg body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, locals_node(p), a, b);
}

/* (:asgn lhs rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_ASGN, cons(a, b));
}

/* (:masgn mlhs=(pre rest post)  mrhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:asgn lhs rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  void_expr_error(p, b);
  return list4((node*)NODE_OP_ASGN, a, nsym(op), b);
}

/* (:int . i) */
static node*
new_int(parser_state *p, const char *s, int base)
{
  return list3((node*)NODE_INT, (node*)strdup(s), nint(base));
}

#ifndef MRB_WITHOUT_FLOAT
/* (:float . i) */
static node*
new_float(parser_state *p, const char *s)
{
  return cons((node*)NODE_FLOAT, (node*)strdup(s));
}
#endif

/* (:str . (s . len)) */
static node*
new_str(parser_state *p, const char *s, size_t len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), nint(len)));
}

/* (:dstr . a) */
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

/* (:str . (s . len)) */
static node*
new_xstr(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_XSTR, cons((node*)strndup(s, len), nint(len)));
}

/* (:xstr . a) */
static node*
new_dxstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DXSTR, a);
}

/* (:dsym . a) */
static node*
new_dsym(parser_state *p, node *a)
{
  return cons((node*)NODE_DSYM, a);
}

/* (:regx . (s . (opt . enc))) */
static node*
new_regx(parser_state *p, const char *p1, const char* p2, const char* p3)
{
  return cons((node*)NODE_REGX, cons((node*)p1, cons((node*)p2, (node*)p3)));
}

/* (:dregx . (a . b)) */
static node*
new_dregx(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DREGX, cons(a, b));
}

/* (:backref . n) */
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, nint(n));
}

/* (:nthref . n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, nint(n));
}

/* (:heredoc . a) */
static node*
new_heredoc(parser_state *p)
{
  parser_heredoc_info *inf = (parser_heredoc_info *)parser_palloc(p, sizeof(parser_heredoc_info));
  return cons((node*)NODE_HEREDOC, (node*)inf);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)NODE_LITERAL_DELIM, 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  return cons((node*)NODE_WORDS, a);
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  return cons((node*)NODE_SYMBOLS, a);
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  void_expr_error(p, recv);
  return new_call(p, recv, intern_cstr(m), 0, 1);
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), list1(list1(arg1)), 1);
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    if (a->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    a->cdr = b;
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  switch ((enum node_type)intn(a->car)) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    if (!a->cdr) a->cdr = cons(0, b);
    else {
      args_with_block(p, a->cdr, b);
    }
    break;
  case NODE_CALL:
  case NODE_FCALL:
  case NODE_SCALL:
    n = a->cdr->cdr->cdr;
    if (!n->car) n->car = cons(0, b);
    else {
      args_with_block(p, n->car, b);
    }
    break;
  default:
    break;
  }
}

static node*
negate_lit(parser_state *p, node *n)
{
  return cons((node*)NODE_NEGATE, n);
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  if (n->cdr) {
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  if (intn(lhs->car) == NODE_LVAR) {
    local_add(p, sym(lhs->cdr));
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  if (intn(lhs->car) == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
      return n;
    }
  }

  return lhs;
}

typedef enum mrb_string_type  string_type;

static node*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  return cons(nint(type), cons((node*)0, cons(nint(paren), nint(term))));
}

static void
end_strterm(parser_state *p)
{
  cons_free(p->lex_strterm->cdr->cdr);
  cons_free(p->lex_strterm->cdr);
  cons_free(p->lex_strterm);
  p->lex_strterm = NULL;
}

static parser_heredoc_info *
parsing_heredoc_inf(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL)
    return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL)
    return;
  if (p->parsing_heredoc == NULL) {
    node *n;
    p->parsing_heredoc = p->heredocs_from_nextline;
    p->lex_strterm_before_heredoc = p->lex_strterm;
    p->lex_strterm = new_strterm(p, parsing_heredoc_inf(p)->type, 0, 0);
    n = p->all_heredocs;
    if (n) {
      while (n->cdr)
        n = n->cdr;
      n->cdr = p->parsing_heredoc;
    }
    else {
      p->all_heredocs = p->parsing_heredoc;
    }
  }
  else {
    node *n, *m;
    m = p->heredocs_from_nextline;
    while (m->cdr)
      m = m->cdr;
    n = p->all_heredocs;
    mrb_assert(n != NULL);
    if (n == p->parsing_heredoc) {
      m->cdr = n;
      p->all_heredocs = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
    else {
      while (n->cdr != p->parsing_heredoc) {
        n = n->cdr;
        mrb_assert(n != NULL);
      }
      m->cdr = n->cdr;
      n->cdr = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
  }
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    p->cmd_start = TRUE;
    end_strterm(p);
    p->lex_strterm = p->lex_strterm_before_heredoc;
    p->lex_strterm_before_heredoc = NULL;
    p->heredoc_end_now = TRUE;
  }
  else {
    /* next heredoc */
    p->lex_strterm->car = nint(parsing_heredoc_inf(p)->type);
  }
}
#define is_strterm_type(p,str_func) (intn((p)->lex_strterm->car) & (str_func))

/* xxx ----------------------------- */



/* Line 189 of yacc.c  */
#line 1157 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-armeabi-v7a-neon-hard/mrbgems/mruby-compiler/core/y.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     keyword_class = 258,
     keyword_module = 259,
     keyword_def = 260,
     keyword_begin = 261,
     keyword_if = 262,
     keyword_unless = 263,
     keyword_while = 264,
     keyword_until = 265,
     keyword_for = 266,
     keyword_undef = 267,
     keyword_rescue = 268,
     keyword_ensure = 269,
     keyword_end = 270,
     keyword_then = 271,
     keyword_elsif = 272,
     keyword_else = 273,
     keyword_case = 274,
     keyword_when = 275,
     keyword_break = 276,
     keyword_next = 277,
     keyword_redo = 278,
     keyword_retry = 279,
     keyword_in = 280,
     keyword_do = 281,
     keyword_do_cond = 282,
     keyword_do_block = 283,
     keyword_do_LAMBDA = 284,
     keyword_return = 285,
     keyword_yield = 286,
     keyword_super = 287,
     keyword_self = 288,
     keyword_nil = 289,
     keyword_true = 290,
     keyword_false = 291,
     keyword_and = 292,
     keyword_or = 293,
     keyword_not = 294,
     modifier_if = 295,
     modifier_unless = 296,
     modifier_while = 297,
     modifier_until = 298,
     modifier_rescue = 299,
     keyword_alias = 300,
     keyword_BEGIN = 301,
     keyword_END = 302,
     keyword__LINE__ = 303,
     keyword__FILE__ = 304,
     keyword__ENCODING__ = 305,
     tIDENTIFIER = 306,
     tFID = 307,
     tGVAR = 308,
     tIVAR = 309,
     tCONSTANT = 310,
     tCVAR = 311,
     tLABEL_TAG = 312,
     tINTEGER = 313,
     tFLOAT = 314,
     tCHAR = 315,
     tXSTRING = 316,
     tREGEXP = 317,
     tSTRING = 318,
     tSTRING_PART = 319,
     tSTRING_MID = 320,
     tNTH_REF = 321,
     tBACK_REF = 322,
     tREGEXP_END = 323,
     tUPLUS = 324,
     tUMINUS = 325,
     tPOW = 326,
     tCMP = 327,
     tEQ = 328,
     tEQQ = 329,
     tNEQ = 330,
     tGEQ = 331,
     tLEQ = 332,
     tANDOP = 333,
     tOROP = 334,
     tMATCH = 335,
     tNMATCH = 336,
     tDOT2 = 337,
     tDOT3 = 338,
     tAREF = 339,
     tASET = 340,
     tLSHFT = 341,
     tRSHFT = 342,
     tCOLON2 = 343,
     tCOLON3 = 344,
     tOP_ASGN = 345,
     tASSOC = 346,
     tLPAREN = 347,
     tLPAREN_ARG = 348,
     tRPAREN = 349,
     tLBRACK = 350,
     tLBRACE = 351,
     tLBRACE_ARG = 352,
     tSTAR = 353,
     tDSTAR = 354,
     tAMPER = 355,
     tLAMBDA = 356,
     tANDDOT = 357,
     tSYMBEG = 358,
     tREGEXP_BEG = 359,
     tWORDS_BEG = 360,
     tSYMBOLS_BEG = 361,
     tSTRING_BEG = 362,
     tXSTRING_BEG = 363,
     tSTRING_DVAR = 364,
     tLAMBEG = 365,
     tHEREDOC_BEG = 366,
     tHEREDOC_END = 367,
     tLITERAL_DELIM = 368,
     tHD_LITERAL_DELIM = 369,
     tHD_STRING_PART = 370,
     tHD_STRING_MID = 371,
     tLOWEST = 372,
     tUMINUS_NUM = 373,
     tLAST_TOKEN = 374
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 1094 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;



/* Line 214 of yacc.c  */
#line 1322 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-armeabi-v7a-neon-hard/mrbgems/mruby-compiler/core/y.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 1334 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-armeabi-v7a-neon-hard/mrbgems/mruby-compiler/core/y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   11742

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  146
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  174
/* YYNRULES -- Number of rules.  */
#define YYNRULES  586
/* YYNRULES -- Number of states.  */
#define YYNSTATES  1025

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   374

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     145,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   132,     2,     2,     2,   130,   125,     2,
     140,   141,   128,   126,   138,   127,   144,   129,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   120,   143,
     122,   118,   121,   119,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   137,     2,   142,   124,     2,   139,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   135,   123,   136,   133,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   131,   134
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    14,    18,    21,
      23,    24,    30,    35,    38,    40,    42,    46,    49,    50,
      55,    58,    62,    66,    70,    74,    78,    83,    85,    89,
      93,    97,   101,   103,   107,   111,   118,   124,   130,   136,
     142,   146,   148,   152,   154,   156,   160,   164,   168,   171,
     173,   175,   177,   179,   181,   186,   187,   193,   196,   200,
     205,   211,   216,   222,   225,   228,   231,   234,   237,   239,
     243,   245,   249,   251,   254,   258,   264,   267,   272,   275,
     280,   282,   286,   288,   292,   295,   299,   301,   304,   306,
     311,   315,   319,   323,   327,   330,   332,   334,   339,   343,
     347,   351,   355,   358,   360,   362,   364,   367,   369,   373,
     375,   377,   379,   381,   383,   385,   387,   389,   390,   395,
     397,   399,   401,   403,   405,   407,   409,   411,   413,   415,
     417,   419,   421,   423,   425,   427,   429,   431,   433,   435,
     437,   439,   441,   443,   445,   447,   449,   451,   453,   455,
     457,   459,   461,   463,   465,   467,   469,   471,   473,   475,
     477,   479,   481,   483,   485,   487,   489,   491,   493,   495,
     497,   499,   501,   503,   505,   507,   509,   511,   513,   515,
     517,   519,   521,   523,   525,   527,   529,   531,   533,   535,
     539,   543,   550,   556,   562,   568,   574,   579,   583,   587,
     591,   595,   599,   603,   607,   611,   615,   620,   625,   628,
     631,   635,   639,   643,   647,   651,   655,   659,   663,   667,
     671,   675,   679,   683,   686,   689,   693,   697,   701,   705,
     712,   719,   721,   723,   726,   731,   734,   736,   740,   744,
     746,   748,   750,   752,   755,   760,   763,   765,   768,   771,
     776,   778,   779,   782,   785,   788,   790,   792,   795,   797,
     800,   804,   809,   813,   818,   821,   823,   825,   827,   829,
     831,   833,   835,   837,   838,   843,   844,   845,   851,   852,
     856,   860,   864,   867,   871,   875,   877,   880,   885,   889,
     892,   894,   897,   898,   899,   905,   912,   919,   920,   921,
     929,   930,   931,   939,   945,   950,   951,   952,   962,   963,
     970,   971,   972,   981,   982,   988,   989,   990,   998,   999,
    1000,  1010,  1012,  1014,  1016,  1018,  1020,  1022,  1024,  1027,
    1029,  1031,  1033,  1039,  1041,  1044,  1046,  1048,  1050,  1054,
    1056,  1060,  1062,  1067,  1074,  1078,  1084,  1087,  1092,  1094,
    1098,  1103,  1106,  1109,  1111,  1114,  1115,  1122,  1131,  1136,
    1143,  1148,  1151,  1158,  1161,  1166,  1173,  1176,  1181,  1184,
    1189,  1191,  1193,  1195,  1199,  1201,  1206,  1208,  1213,  1215,
    1219,  1221,  1223,  1228,  1230,  1234,  1238,  1239,  1245,  1248,
    1253,  1259,  1265,  1268,  1273,  1278,  1282,  1286,  1290,  1293,
    1295,  1300,  1301,  1307,  1308,  1314,  1320,  1322,  1324,  1331,
    1333,  1335,  1337,  1339,  1342,  1344,  1347,  1349,  1351,  1353,
    1355,  1357,  1359,  1361,  1364,  1368,  1370,  1373,  1375,  1376,
    1381,  1383,  1386,  1389,  1393,  1396,  1400,  1402,  1404,  1407,
    1409,  1412,  1414,  1417,  1419,  1420,  1425,  1428,  1432,  1434,
    1439,  1442,  1444,  1446,  1448,  1450,  1452,  1455,  1458,  1462,
    1464,  1466,  1469,  1472,  1474,  1476,  1478,  1480,  1482,  1484,
    1486,  1488,  1490,  1492,  1494,  1496,  1498,  1500,  1502,  1504,
    1505,  1506,  1511,  1515,  1518,  1521,  1524,  1526,  1529,  1531,
    1533,  1537,  1539,  1543,  1545,  1547,  1550,  1552,  1557,  1560,
    1563,  1565,  1568,  1569,  1576,  1585,  1590,  1597,  1602,  1609,
    1612,  1617,  1624,  1627,  1632,  1635,  1640,  1642,  1643,  1645,
    1647,  1649,  1651,  1653,  1655,  1657,  1661,  1663,  1667,  1670,
    1673,  1676,  1678,  1682,  1684,  1688,  1690,  1692,  1695,  1697,
    1699,  1701,  1704,  1707,  1709,  1711,  1712,  1717,  1719,  1722,
    1724,  1728,  1732,  1736,  1740,  1743,  1745,  1747,  1749,  1751,
    1753,  1755,  1757,  1759,  1761,  1763,  1765,  1767,  1769,  1771,
    1773,  1775,  1776,  1778,  1779,  1781,  1784,  1787,  1788,  1790,
    1792,  1794,  1796,  1798,  1800,  1802,  1805
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     147,     0,    -1,    -1,   148,   149,    -1,   150,   311,    -1,
     319,    -1,   151,    -1,   150,   318,   151,    -1,     1,   151,
      -1,   156,    -1,    -1,    46,   152,   135,   149,   136,    -1,
     154,   246,   222,   249,    -1,   155,   311,    -1,   319,    -1,
     156,    -1,   155,   318,   156,    -1,     1,   156,    -1,    -1,
      45,   178,   157,   178,    -1,    12,   179,    -1,   156,    40,
     161,    -1,   156,    41,   161,    -1,   156,    42,   161,    -1,
     156,    43,   161,    -1,   156,    44,   156,    -1,    47,   135,
     154,   136,    -1,   158,    -1,   167,   118,   162,    -1,   174,
     118,   196,    -1,   167,   118,   183,    -1,   167,   118,   196,
      -1,   160,    -1,   174,   118,   159,    -1,   270,    90,   159,
      -1,   218,   137,   188,   314,    90,   159,    -1,   218,   309,
      51,    90,   159,    -1,   218,   309,    55,    90,   159,    -1,
     218,    88,    55,    90,   162,    -1,   218,    88,    51,    90,
     159,    -1,   272,    90,   159,    -1,   162,    -1,   162,    44,
     156,    -1,   158,    -1,   162,    -1,   160,    37,   160,    -1,
     160,    38,   160,    -1,    39,   312,   160,    -1,   132,   162,
      -1,   183,    -1,   160,    -1,   166,    -1,   163,    -1,   239,
      -1,   239,   310,   306,   190,    -1,    -1,    97,   165,   230,
     154,   136,    -1,   305,   190,    -1,   305,   190,   164,    -1,
     218,   309,   306,   190,    -1,   218,   309,   306,   190,   164,
      -1,   218,    88,   306,   190,    -1,   218,    88,   306,   190,
     164,    -1,    32,   190,    -1,    31,   190,    -1,    30,   189,
      -1,    21,   189,    -1,    22,   189,    -1,   169,    -1,    92,
     168,   313,    -1,   169,    -1,    92,   168,   313,    -1,   171,
      -1,   171,   170,    -1,   171,    98,   173,    -1,   171,    98,
     173,   138,   172,    -1,   171,    98,    -1,   171,    98,   138,
     172,    -1,    98,   173,    -1,    98,   173,   138,   172,    -1,
      98,    -1,    98,   138,   172,    -1,   173,    -1,    92,   168,
     313,    -1,   170,   138,    -1,   171,   170,   138,    -1,   170,
      -1,   171,   170,    -1,   269,    -1,   218,   137,   188,   314,
      -1,   218,   309,    51,    -1,   218,    88,    51,    -1,   218,
     309,    55,    -1,   218,    88,    55,    -1,    89,    55,    -1,
     272,    -1,   269,    -1,   218,   137,   188,   314,    -1,   218,
     309,    51,    -1,   218,    88,    51,    -1,   218,   309,    55,
      -1,   218,    88,    55,    -1,    89,    55,    -1,   272,    -1,
      51,    -1,    55,    -1,    89,   175,    -1,   175,    -1,   218,
      88,   175,    -1,    51,    -1,    55,    -1,    52,    -1,   181,
      -1,   182,    -1,   177,    -1,   265,    -1,   178,    -1,    -1,
     179,   138,   180,   178,    -1,   123,    -1,   124,    -1,   125,
      -1,    72,    -1,    73,    -1,    74,    -1,    80,    -1,    81,
      -1,   121,    -1,    76,    -1,   122,    -1,    77,    -1,    75,
      -1,    86,    -1,    87,    -1,   126,    -1,   127,    -1,   128,
      -1,    98,    -1,   129,    -1,   130,    -1,    71,    -1,    99,
      -1,   132,    -1,   133,    -1,    69,    -1,    70,    -1,    84,
      -1,    85,    -1,   139,    -1,    48,    -1,    49,    -1,    50,
      -1,    46,    -1,    47,    -1,    45,    -1,    37,    -1,     6,
      -1,    21,    -1,    19,    -1,     3,    -1,     5,    -1,    26,
      -1,    18,    -1,    17,    -1,    15,    -1,    14,    -1,    36,
      -1,    11,    -1,    25,    -1,     4,    -1,    22,    -1,    34,
      -1,    39,    -1,    38,    -1,    23,    -1,    13,    -1,    24,
      -1,    30,    -1,    33,    -1,    32,    -1,    16,    -1,    35,
      -1,    12,    -1,    20,    -1,    31,    -1,     7,    -1,     8,
      -1,     9,    -1,    10,    -1,   174,   118,   185,    -1,   270,
      90,   185,    -1,   218,   137,   188,   314,    90,   185,    -1,
     218,   309,    51,    90,   185,    -1,   218,   309,    55,    90,
     185,    -1,   218,    88,    51,    90,   185,    -1,   218,    88,
      55,    90,   185,    -1,    89,    55,    90,   185,    -1,   272,
      90,   185,    -1,   183,    82,   183,    -1,   183,    83,   183,
      -1,   183,   126,   183,    -1,   183,   127,   183,    -1,   183,
     128,   183,    -1,   183,   129,   183,    -1,   183,   130,   183,
      -1,   183,    71,   183,    -1,   131,    58,    71,   183,    -1,
     131,    59,    71,   183,    -1,    69,   183,    -1,    70,   183,
      -1,   183,   123,   183,    -1,   183,   124,   183,    -1,   183,
     125,   183,    -1,   183,    72,   183,    -1,   183,   121,   183,
      -1,   183,    76,   183,    -1,   183,   122,   183,    -1,   183,
      77,   183,    -1,   183,    73,   183,    -1,   183,    74,   183,
      -1,   183,    75,   183,    -1,   183,    80,   183,    -1,   183,
      81,   183,    -1,   132,   183,    -1,   133,   183,    -1,   183,
      86,   183,    -1,   183,    87,   183,    -1,   183,    78,   183,
      -1,   183,    79,   183,    -1,   183,   119,   183,   312,   120,
     183,    -1,   183,   119,   183,   312,    57,   183,    -1,   197,
      -1,   319,    -1,   195,   315,    -1,   195,   194,   303,   315,
      -1,   303,   315,    -1,   183,    -1,   183,    44,   183,    -1,
     140,   188,   313,    -1,   319,    -1,   186,    -1,   319,    -1,
     189,    -1,   195,   138,    -1,   195,   194,   303,   138,    -1,
     303,   138,    -1,   166,    -1,   195,   193,    -1,   303,   193,
      -1,   195,   194,   303,   193,    -1,   192,    -1,    -1,   191,
     189,    -1,   100,   183,    -1,   194,   192,    -1,   319,    -1,
     138,    -1,   138,   258,    -1,   183,    -1,    98,   183,    -1,
     195,   194,   183,    -1,   195,   194,    98,   183,    -1,   195,
     194,   183,    -1,   195,   194,    98,   183,    -1,    98,   183,
      -1,   250,    -1,   251,    -1,   255,    -1,   256,    -1,   257,
      -1,   271,    -1,   272,    -1,    52,    -1,    -1,     6,   198,
     153,    15,    -1,    -1,    -1,    93,   199,   156,   200,   313,
      -1,    -1,    93,   201,   313,    -1,    92,   154,   141,    -1,
     218,    88,    55,    -1,    89,    55,    -1,    95,   184,   142,
      -1,    96,   302,   136,    -1,    30,    -1,    31,   187,    -1,
      39,   140,   160,   313,    -1,    39,   140,   313,    -1,   305,
     241,    -1,   240,    -1,   240,   241,    -1,    -1,    -1,   101,
     202,   235,   203,   236,    -1,     7,   161,   219,   154,   221,
      15,    -1,     8,   161,   219,   154,   222,    15,    -1,    -1,
      -1,     9,   204,   161,   220,   205,   154,    15,    -1,    -1,
      -1,    10,   206,   161,   220,   207,   154,    15,    -1,    19,
     161,   311,   244,    15,    -1,    19,   311,   244,    15,    -1,
      -1,    -1,    11,   223,    25,   208,   161,   220,   209,   154,
      15,    -1,    -1,     3,   176,   273,   210,   153,    15,    -1,
      -1,    -1,     3,    86,   160,   211,   316,   212,   153,    15,
      -1,    -1,     4,   176,   213,   153,    15,    -1,    -1,    -1,
       5,   177,   214,   215,   275,   153,    15,    -1,    -1,    -1,
       5,   300,   308,   216,   177,   217,   275,   153,    15,    -1,
      21,    -1,    22,    -1,    23,    -1,    24,    -1,   197,    -1,
     316,    -1,    16,    -1,   316,    16,    -1,   316,    -1,    27,
      -1,   222,    -1,    17,   161,   219,   154,   221,    -1,   319,
      -1,    18,   154,    -1,   174,    -1,   167,    -1,   287,    -1,
      92,   226,   313,    -1,   224,    -1,   225,   138,   224,    -1,
     225,    -1,   225,   138,    98,   287,    -1,   225,   138,    98,
     287,   138,   225,    -1,   225,   138,    98,    -1,   225,   138,
      98,   138,   225,    -1,    98,   287,    -1,    98,   287,   138,
     225,    -1,    98,    -1,    98,   138,   225,    -1,   279,   138,
     282,   299,    -1,   279,   299,    -1,   282,   299,    -1,   298,
      -1,   138,   227,    -1,    -1,   289,   138,   293,   138,   296,
     228,    -1,   289,   138,   293,   138,   296,   138,   289,   228,
      -1,   289,   138,   293,   228,    -1,   289,   138,   293,   138,
     289,   228,    -1,   289,   138,   296,   228,    -1,   289,   138,
      -1,   289,   138,   296,   138,   289,   228,    -1,   289,   228,
      -1,   293,   138,   296,   228,    -1,   293,   138,   296,   138,
     289,   228,    -1,   293,   228,    -1,   293,   138,   289,   228,
      -1,   296,   228,    -1,   296,   138,   289,   228,    -1,   227,
      -1,   319,    -1,   231,    -1,   123,   232,   123,    -1,    79,
      -1,   123,   229,   232,   123,    -1,   312,    -1,   312,   143,
     233,   312,    -1,   234,    -1,   233,   138,   234,    -1,    51,
      -1,   286,    -1,   140,   285,   232,   141,    -1,   285,    -1,
     110,   154,   136,    -1,    29,   153,    15,    -1,    -1,    28,
     238,   230,   153,    15,    -1,   166,   237,    -1,   239,   310,
     306,   187,    -1,   239,   310,   306,   187,   241,    -1,   239,
     310,   306,   190,   237,    -1,   305,   186,    -1,   218,   309,
     306,   187,    -1,   218,    88,   306,   186,    -1,   218,    88,
     307,    -1,   218,   309,   186,    -1,   218,    88,   186,    -1,
      32,   186,    -1,    32,    -1,   218,   137,   188,   314,    -1,
      -1,   135,   242,   230,   154,   136,    -1,    -1,    26,   243,
     230,   153,    15,    -1,    20,   195,   219,   154,   245,    -1,
     222,    -1,   244,    -1,    13,   247,   248,   219,   154,   246,
      -1,   319,    -1,   183,    -1,   196,    -1,   319,    -1,    91,
     174,    -1,   319,    -1,    14,   154,    -1,   319,    -1,   268,
      -1,   264,    -1,   263,    -1,   267,    -1,    60,    -1,    63,
      -1,   107,    63,    -1,   107,   252,    63,    -1,   253,    -1,
     252,   253,    -1,    65,    -1,    -1,    64,   254,   154,   136,
      -1,   113,    -1,   114,   258,    -1,   108,    61,    -1,   108,
     252,    61,    -1,   104,    62,    -1,   104,   252,    62,    -1,
     111,    -1,   259,    -1,   258,   259,    -1,   112,    -1,   260,
     112,    -1,   261,    -1,   260,   261,    -1,   116,    -1,    -1,
     115,   262,   154,   136,    -1,   105,    63,    -1,   105,   252,
      63,    -1,   265,    -1,   103,   107,   252,    63,    -1,   103,
     266,    -1,   177,    -1,    54,    -1,    53,    -1,    56,    -1,
      63,    -1,   107,    63,    -1,   106,    63,    -1,   106,   252,
      63,    -1,    58,    -1,    59,    -1,   131,    58,    -1,   131,
      59,    -1,    51,    -1,    54,    -1,    53,    -1,    56,    -1,
      55,    -1,   269,    -1,   269,    -1,    34,    -1,    33,    -1,
      35,    -1,    36,    -1,    49,    -1,    48,    -1,    50,    -1,
      66,    -1,    67,    -1,    -1,    -1,   122,   274,   161,   316,
      -1,   140,   285,   313,    -1,   285,   316,    -1,    51,    57,
      -1,   276,   183,    -1,   276,    -1,   276,   218,    -1,   276,
      -1,   278,    -1,   279,   138,   278,    -1,   277,    -1,   280,
     138,   277,    -1,    71,    -1,    99,    -1,   281,    51,    -1,
     281,    -1,   280,   138,   282,   299,    -1,   280,   299,    -1,
     282,   299,    -1,   298,    -1,   138,   283,    -1,    -1,   289,
     138,   294,   138,   296,   284,    -1,   289,   138,   294,   138,
     296,   138,   289,   284,    -1,   289,   138,   294,   284,    -1,
     289,   138,   294,   138,   289,   284,    -1,   289,   138,   296,
     284,    -1,   289,   138,   296,   138,   289,   284,    -1,   289,
     284,    -1,   294,   138,   296,   284,    -1,   294,   138,   296,
     138,   289,   284,    -1,   294,   284,    -1,   294,   138,   289,
     284,    -1,   296,   284,    -1,   296,   138,   289,   284,    -1,
     283,    -1,    -1,    55,    -1,    54,    -1,    53,    -1,    56,
      -1,   286,    -1,    51,    -1,   287,    -1,    92,   226,   313,
      -1,   288,    -1,   289,   138,   288,    -1,    51,   118,    -1,
     290,   183,    -1,   290,   218,    -1,   292,    -1,   293,   138,
     292,    -1,   291,    -1,   294,   138,   291,    -1,   128,    -1,
      98,    -1,   295,    51,    -1,   295,    -1,   125,    -1,   100,
      -1,   297,    51,    -1,   138,   298,    -1,   319,    -1,   271,
      -1,    -1,   140,   301,   160,   313,    -1,   319,    -1,   303,
     315,    -1,   304,    -1,   303,   138,   304,    -1,   183,    91,
     183,    -1,    51,    57,   183,    -1,   251,    57,   183,    -1,
      99,   183,    -1,    51,    -1,    55,    -1,    52,    -1,    51,
      -1,    55,    -1,    52,    -1,   181,    -1,    51,    -1,    52,
      -1,   181,    -1,   144,    -1,    88,    -1,   144,    -1,   102,
      -1,   309,    -1,    88,    -1,    -1,   318,    -1,    -1,   317,
      -1,   312,   141,    -1,   312,   142,    -1,    -1,   317,    -1,
     194,    -1,   143,    -1,   317,    -1,   259,    -1,   145,    -1,
     316,    -1,   318,   316,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1251,  1251,  1251,  1262,  1268,  1272,  1277,  1281,  1287,
    1289,  1288,  1300,  1327,  1333,  1337,  1342,  1346,  1352,  1352,
    1356,  1360,  1364,  1368,  1372,  1376,  1380,  1385,  1386,  1390,
    1394,  1398,  1402,  1405,  1409,  1413,  1417,  1421,  1425,  1430,
    1434,  1441,  1442,  1446,  1450,  1451,  1455,  1459,  1463,  1467,
    1470,  1479,  1480,  1483,  1484,  1491,  1490,  1503,  1507,  1512,
    1516,  1521,  1525,  1530,  1534,  1538,  1542,  1546,  1552,  1556,
    1562,  1563,  1569,  1573,  1577,  1581,  1585,  1589,  1593,  1597,
    1601,  1605,  1611,  1612,  1618,  1622,  1628,  1632,  1638,  1642,
    1646,  1650,  1654,  1658,  1664,  1670,  1677,  1681,  1685,  1689,
    1693,  1697,  1703,  1709,  1716,  1720,  1723,  1727,  1731,  1738,
    1739,  1740,  1741,  1746,  1753,  1754,  1757,  1761,  1761,  1767,
    1768,  1769,  1770,  1771,  1772,  1773,  1774,  1775,  1776,  1777,
    1778,  1779,  1780,  1781,  1782,  1783,  1784,  1785,  1786,  1787,
    1788,  1789,  1790,  1791,  1792,  1793,  1794,  1795,  1796,  1799,
    1799,  1799,  1800,  1800,  1801,  1801,  1801,  1802,  1802,  1802,
    1802,  1803,  1803,  1803,  1804,  1804,  1804,  1805,  1805,  1805,
    1805,  1806,  1806,  1806,  1806,  1807,  1807,  1807,  1807,  1808,
    1808,  1808,  1808,  1809,  1809,  1809,  1809,  1810,  1810,  1813,
    1817,  1821,  1825,  1829,  1833,  1837,  1842,  1847,  1852,  1856,
    1860,  1864,  1868,  1872,  1876,  1880,  1884,  1888,  1892,  1896,
    1900,  1904,  1908,  1912,  1916,  1920,  1924,  1928,  1932,  1936,
    1940,  1944,  1948,  1952,  1956,  1960,  1964,  1968,  1972,  1976,
    1980,  1984,  1990,  1991,  1996,  2000,  2007,  2011,  2019,  2025,
    2026,  2029,  2030,  2031,  2036,  2041,  2048,  2054,  2059,  2064,
    2069,  2076,  2076,  2087,  2093,  2097,  2103,  2104,  2107,  2113,
    2119,  2124,  2131,  2136,  2141,  2148,  2149,  2150,  2151,  2152,
    2153,  2154,  2155,  2160,  2159,  2171,  2175,  2170,  2180,  2180,
    2184,  2188,  2192,  2196,  2201,  2206,  2210,  2214,  2218,  2222,
    2226,  2227,  2233,  2239,  2232,  2251,  2259,  2267,  2267,  2267,
    2274,  2274,  2274,  2281,  2287,  2292,  2294,  2291,  2303,  2301,
    2317,  2322,  2315,  2337,  2335,  2350,  2354,  2349,  2369,  2375,
    2368,  2390,  2394,  2398,  2402,  2408,  2415,  2416,  2417,  2420,
    2421,  2424,  2425,  2433,  2434,  2440,  2444,  2447,  2451,  2457,
    2461,  2467,  2471,  2475,  2479,  2483,  2487,  2491,  2495,  2499,
    2505,  2509,  2513,  2517,  2523,  2528,  2533,  2537,  2541,  2545,
    2549,  2553,  2557,  2561,  2565,  2569,  2573,  2577,  2581,  2585,
    2589,  2595,  2596,  2603,  2607,  2611,  2618,  2622,  2628,  2629,
    2632,  2637,  2640,  2644,  2650,  2654,  2661,  2660,  2673,  2683,
    2687,  2692,  2699,  2703,  2707,  2711,  2715,  2719,  2723,  2727,
    2731,  2738,  2737,  2750,  2749,  2763,  2771,  2780,  2783,  2790,
    2793,  2797,  2798,  2801,  2805,  2808,  2812,  2815,  2816,  2817,
    2818,  2821,  2822,  2823,  2827,  2833,  2834,  2840,  2845,  2844,
    2855,  2859,  2865,  2869,  2875,  2879,  2885,  2888,  2889,  2892,
    2898,  2904,  2905,  2908,  2915,  2914,  2928,  2932,  2939,  2944,
    2951,  2957,  2958,  2959,  2960,  2961,  2965,  2971,  2975,  2981,
    2982,  2983,  2987,  2993,  2997,  3001,  3005,  3009,  3015,  3021,
    3025,  3029,  3033,  3037,  3041,  3049,  3056,  3067,  3068,  3072,
    3076,  3075,  3091,  3097,  3103,  3106,  3111,  3117,  3121,  3127,
    3131,  3137,  3141,  3147,  3148,  3151,  3155,  3161,  3165,  3169,
    3173,  3179,  3184,  3189,  3193,  3197,  3201,  3205,  3209,  3213,
    3217,  3221,  3225,  3229,  3233,  3237,  3241,  3246,  3252,  3257,
    3262,  3267,  3274,  3278,  3285,  3289,  3295,  3299,  3305,  3312,
    3319,  3326,  3330,  3336,  3340,  3346,  3347,  3350,  3355,  3362,
    3363,  3366,  3372,  3376,  3382,  3387,  3387,  3412,  3413,  3419,
    3424,  3430,  3436,  3441,  3451,  3458,  3459,  3460,  3463,  3464,
    3465,  3466,  3469,  3470,  3471,  3474,  3475,  3478,  3482,  3488,
    3489,  3495,  3496,  3499,  3500,  3503,  3506,  3509,  3510,  3511,
    3514,  3515,  3516,  3519,  3526,  3527,  3531
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "keyword_class", "keyword_module",
  "keyword_def", "keyword_begin", "keyword_if", "keyword_unless",
  "keyword_while", "keyword_until", "keyword_for", "keyword_undef",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_then",
  "keyword_elsif", "keyword_else", "keyword_case", "keyword_when",
  "keyword_break", "keyword_next", "keyword_redo", "keyword_retry",
  "keyword_in", "keyword_do", "keyword_do_cond", "keyword_do_block",
  "keyword_do_LAMBDA", "keyword_return", "keyword_yield", "keyword_super",
  "keyword_self", "keyword_nil", "keyword_true", "keyword_false",
  "keyword_and", "keyword_or", "keyword_not", "modifier_if",
  "modifier_unless", "modifier_while", "modifier_until", "modifier_rescue",
  "keyword_alias", "keyword_BEGIN", "keyword_END", "keyword__LINE__",
  "keyword__FILE__", "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR",
  "tIVAR", "tCONSTANT", "tCVAR", "tLABEL_TAG", "tINTEGER", "tFLOAT",
  "tCHAR", "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
  "tNTH_REF", "tBACK_REF", "tREGEXP_END", "tUPLUS", "tUMINUS", "tPOW",
  "tCMP", "tEQ", "tEQQ", "tNEQ", "tGEQ", "tLEQ", "tANDOP", "tOROP",
  "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tAREF", "tASET", "tLSHFT",
  "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC", "tLPAREN",
  "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG", "tSTAR",
  "tDSTAR", "tAMPER", "tLAMBDA", "tANDDOT", "tSYMBEG", "tREGEXP_BEG",
  "tWORDS_BEG", "tSYMBOLS_BEG", "tSTRING_BEG", "tXSTRING_BEG",
  "tSTRING_DVAR", "tLAMBEG", "tHEREDOC_BEG", "tHEREDOC_END",
  "tLITERAL_DELIM", "tHD_LITERAL_DELIM", "tHD_STRING_PART",
  "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'",
  "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'",
  "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "','", "'`'", "'('", "')'",
  "']'", "';'", "'.'", "'\\n'", "$accept", "program", "$@1",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "stmt", "$@3", "command_asgn", "command_rhs", "expr",
  "expr_value", "command_call", "block_command", "cmd_brace_block", "$@4",
  "command", "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list",
  "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym",
  "undef_list", "$@5", "op", "reswords", "arg", "aref_args", "arg_rhs",
  "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@6", "block_arg", "opt_block_arg", "comma", "args",
  "mrhs", "primary", "@7", "@8", "$@9", "$@10", "@11", "@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "$@18", "@19", "@20", "@21", "@22",
  "@23", "@24", "@25", "@26", "primary_value", "then", "do", "if_tail",
  "opt_else", "for_var", "f_marg", "f_marg_list", "f_margs",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "f_larglist", "lambda_body", "do_block", "$@27", "block_call",
  "method_call", "brace_block", "@28", "@29", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_rep", "string_interp", "@30", "xstring", "regexp", "heredoc",
  "heredoc_bodies", "heredoc_body", "heredoc_string_rep",
  "heredoc_string_interp", "@31", "words", "symbol", "basic_symbol", "sym",
  "symbols", "numeric", "variable", "var_lhs", "var_ref", "backref",
  "superclass", "$@32", "f_arglist", "f_label", "f_kw", "f_block_kw",
  "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest", "args_tail",
  "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg", "f_arg_item",
  "f_arg", "f_opt_asgn", "f_opt", "f_block_opt", "f_block_optarg",
  "f_optarg", "restarg_mark", "f_rest_arg", "blkarg_mark", "f_block_arg",
  "opt_f_block_arg", "singleton", "$@33", "assoc_list", "assocs", "assoc",
  "operation", "operation2", "operation3", "dot_or_colon", "call_op",
  "call_op2", "opt_terms", "opt_nl", "rparen", "rbracket", "trailer",
  "term", "nl", "terms", "none", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,    61,    63,
      58,    62,    60,   124,    94,    38,    43,    45,    42,    47,
      37,   373,    33,   126,   374,   123,   125,    91,    44,    96,
      40,    41,    93,    59,    46,    10
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   146,   148,   147,   149,   150,   150,   150,   150,   151,
     152,   151,   153,   154,   155,   155,   155,   155,   157,   156,
     156,   156,   156,   156,   156,   156,   156,   156,   156,   156,
     156,   156,   156,   158,   158,   158,   158,   158,   158,   158,
     158,   159,   159,   159,   160,   160,   160,   160,   160,   160,
     161,   162,   162,   163,   163,   165,   164,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   167,   167,
     168,   168,   169,   169,   169,   169,   169,   169,   169,   169,
     169,   169,   170,   170,   171,   171,   172,   172,   173,   173,
     173,   173,   173,   173,   173,   173,   174,   174,   174,   174,
     174,   174,   174,   174,   175,   175,   176,   176,   176,   177,
     177,   177,   177,   177,   178,   178,   179,   180,   179,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   184,   184,   184,   184,   185,   185,   186,   187,
     187,   188,   188,   188,   188,   188,   189,   189,   189,   189,
     189,   191,   190,   192,   193,   193,   194,   194,   195,   195,
     195,   195,   196,   196,   196,   197,   197,   197,   197,   197,
     197,   197,   197,   198,   197,   199,   200,   197,   201,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   197,   202,   203,   197,   197,   197,   204,   205,   197,
     206,   207,   197,   197,   197,   208,   209,   197,   210,   197,
     211,   212,   197,   213,   197,   214,   215,   197,   216,   217,
     197,   197,   197,   197,   197,   218,   219,   219,   219,   220,
     220,   221,   221,   222,   222,   223,   223,   224,   224,   225,
     225,   226,   226,   226,   226,   226,   226,   226,   226,   226,
     227,   227,   227,   227,   228,   228,   229,   229,   229,   229,
     229,   229,   229,   229,   229,   229,   229,   229,   229,   229,
     229,   230,   230,   231,   231,   231,   232,   232,   233,   233,
     234,   234,   235,   235,   236,   236,   238,   237,   239,   239,
     239,   239,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   242,   241,   243,   241,   244,   245,   245,   246,   246,
     247,   247,   247,   248,   248,   249,   249,   250,   250,   250,
     250,   251,   251,   251,   251,   252,   252,   253,   254,   253,
     253,   253,   255,   255,   256,   256,   257,   258,   258,   259,
     259,   260,   260,   261,   262,   261,   263,   263,   264,   264,
     265,   266,   266,   266,   266,   266,   266,   267,   267,   268,
     268,   268,   268,   269,   269,   269,   269,   269,   270,   271,
     271,   271,   271,   271,   271,   271,   271,   272,   272,   273,
     274,   273,   275,   275,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   281,   281,   282,   282,   283,   283,   283,
     283,   284,   284,   285,   285,   285,   285,   285,   285,   285,
     285,   285,   285,   285,   285,   285,   285,   285,   286,   286,
     286,   286,   287,   287,   288,   288,   289,   289,   290,   291,
     292,   293,   293,   294,   294,   295,   295,   296,   296,   297,
     297,   298,   299,   299,   300,   301,   300,   302,   302,   303,
     303,   304,   304,   304,   304,   305,   305,   305,   306,   306,
     306,   306,   307,   307,   307,   308,   308,   309,   309,   310,
     310,   311,   311,   312,   312,   313,   314,   315,   315,   315,
     316,   316,   316,   317,   318,   318,   319
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     3,     3,     6,     5,     5,     5,     5,
       3,     1,     3,     1,     1,     3,     3,     3,     2,     1,
       1,     1,     1,     1,     4,     0,     5,     2,     3,     4,
       5,     4,     5,     2,     2,     2,     2,     2,     1,     3,
       1,     3,     1,     2,     3,     5,     2,     4,     2,     4,
       1,     3,     1,     3,     2,     3,     1,     2,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     1,     2,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     6,     5,     5,     5,     5,     4,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     4,     4,     2,     2,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     2,     3,     3,     3,     3,     6,
       6,     1,     1,     2,     4,     2,     1,     3,     3,     1,
       1,     1,     1,     2,     4,     2,     1,     2,     2,     4,
       1,     0,     2,     2,     2,     1,     1,     2,     1,     2,
       3,     4,     3,     4,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     0,     0,     5,     0,     3,
       3,     3,     2,     3,     3,     1,     2,     4,     3,     2,
       1,     2,     0,     0,     5,     6,     6,     0,     0,     7,
       0,     0,     7,     5,     4,     0,     0,     9,     0,     6,
       0,     0,     8,     0,     5,     0,     0,     7,     0,     0,
       9,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     5,     1,     2,     1,     1,     1,     3,     1,
       3,     1,     4,     6,     3,     5,     2,     4,     1,     3,
       4,     2,     2,     1,     2,     0,     6,     8,     4,     6,
       4,     2,     6,     2,     4,     6,     2,     4,     2,     4,
       1,     1,     1,     3,     1,     4,     1,     4,     1,     3,
       1,     1,     4,     1,     3,     3,     0,     5,     2,     4,
       5,     5,     2,     4,     4,     3,     3,     3,     2,     1,
       4,     0,     5,     0,     5,     5,     1,     1,     6,     1,
       1,     1,     1,     2,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     2,     3,     1,     2,     1,     0,     4,
       1,     2,     2,     3,     2,     3,     1,     1,     2,     1,
       2,     1,     2,     1,     0,     4,     2,     3,     1,     4,
       2,     1,     1,     1,     1,     1,     2,     2,     3,     1,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       0,     4,     3,     2,     2,     2,     1,     2,     1,     1,
       3,     1,     3,     1,     1,     2,     1,     4,     2,     2,
       1,     2,     0,     6,     8,     4,     6,     4,     6,     2,
       4,     6,     2,     4,     2,     4,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     3,     2,     2,
       2,     1,     3,     1,     3,     1,     1,     2,     1,     1,
       1,     2,     2,     1,     1,     0,     4,     1,     2,     1,
       3,     3,     3,     3,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     0,     1,     2,     2,     0,     1,     1,
       1,     1,     1,     1,     1,     2,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   273,     0,
       0,   297,   300,     0,     0,   571,   321,   322,   323,   324,
     285,   251,   399,   471,   470,   472,   473,   573,     0,    10,
       0,   475,   474,   476,   463,   272,   465,   464,   467,   466,
     459,   460,   421,   422,   477,   478,     0,     0,     0,     0,
     275,   586,   586,    80,   292,     0,     0,     0,     0,     0,
       0,   436,     0,     0,     0,     3,   571,     6,     9,    27,
      32,    44,    52,    51,     0,    68,     0,    72,    82,     0,
      49,   231,     0,    53,   290,   265,   266,   267,   268,   269,
     419,   418,   448,   420,   417,   469,     0,   270,   271,   251,
       5,     8,   321,   322,   285,   586,   399,     0,   104,   105,
       0,     0,     0,     0,   107,   479,   325,     0,   469,   271,
       0,   313,   159,   169,   160,   156,   185,   186,   187,   188,
     167,   182,   175,   165,   164,   180,   163,   162,   158,   183,
     157,   170,   174,   176,   168,   161,   177,   184,   179,   178,
     171,   181,   166,   155,   173,   172,   154,   152,   153,   149,
     150,   151,   109,   111,   110,   144,   145,   140,   122,   123,
     124,   131,   128,   130,   125,   126,   146,   147,   132,   133,
     137,   141,   127,   129,   119,   120,   121,   134,   135,   136,
     138,   139,   142,   143,   148,   545,   315,   112,   113,   544,
       0,     0,     0,    50,     0,     0,     0,   469,     0,   271,
       0,     0,     0,     0,   336,   335,     0,     0,   469,   271,
     178,   171,   181,   166,   149,   150,   151,   109,   110,     0,
     114,   116,    20,   115,   439,   444,   443,   580,   583,   571,
     582,     0,   441,     0,   584,   581,   572,   555,     0,     0,
       0,     0,   246,   258,    66,   250,   586,   266,   586,   549,
      67,    65,   586,   240,   286,    64,     0,   239,   398,    63,
     573,     0,   574,    18,     0,     0,   208,     0,   209,   282,
       0,     0,     0,   571,    15,   573,    70,    14,     0,   573,
       0,   577,   577,   232,     0,     0,   577,   547,     0,     0,
      78,     0,    88,    95,   517,   453,   452,   454,   455,     0,
     451,   450,   434,   428,   427,   430,     0,     0,   425,   446,
       0,   457,     0,   423,     0,   432,     0,   461,   462,    48,
     223,   224,     4,   572,     0,     0,     0,     0,     0,     0,
       0,   386,   388,     0,    84,     0,    76,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   568,   586,   567,     0,
     570,   569,     0,   403,   401,   291,     0,     0,   392,    57,
     289,   310,   104,   105,   106,   461,   462,   480,   308,     0,
     586,     0,     0,     0,   316,   566,   565,   318,     0,   586,
     282,   327,     0,   326,     0,     0,   586,     0,     0,     0,
       0,     0,     0,   282,     0,   586,     0,   305,     0,   117,
       0,     0,   440,   442,     0,     0,   585,     0,   259,   554,
     253,     0,   256,   247,     0,   255,     0,   256,   248,     0,
     573,   242,   586,   586,   241,   252,   573,     0,   288,    47,
       0,     0,     0,     0,     0,     0,    17,   573,   280,    13,
     572,    69,   276,   279,   283,   579,   233,   578,   579,   235,
     284,   548,    94,    86,     0,    81,     0,     0,   586,     0,
     523,   520,   519,   518,   521,   493,     0,   536,   494,   540,
     539,   535,   517,   293,   486,   491,   586,   496,   586,   516,
     383,   522,   524,   526,   502,     0,   533,   502,   538,   502,
       0,   500,   456,     0,     0,   431,   437,   435,   426,   447,
     458,   424,   433,     0,     0,     7,    21,    22,    23,    24,
      25,    45,    46,   586,     0,    28,    30,     0,    31,   573,
       0,    74,    85,    43,    33,    41,     0,   236,   189,    29,
       0,   271,   205,   213,   218,   219,   220,   215,   217,   227,
     228,   221,   222,   198,   199,   225,   226,   573,   214,   216,
     210,   211,   212,   200,   201,   202,   203,   204,   558,   563,
     559,   564,   397,   251,   395,   573,   558,   560,   559,   561,
     396,   586,   558,   559,   251,   586,   586,    34,   236,   190,
      40,   197,    55,    58,     0,     0,     0,   104,   105,   108,
       0,   573,   586,     0,   573,   517,     0,   274,   586,   586,
     409,   586,   328,   562,   281,   573,   558,   559,   586,   330,
     298,   329,   301,   562,   281,   573,   558,   559,     0,     0,
       0,     0,   258,     0,   304,   552,   551,   257,     0,   260,
     254,   586,   553,   550,   238,   256,     0,   245,   287,   575,
      19,     0,    26,   196,    71,    16,   573,   577,    87,    79,
     562,    93,   573,   558,   559,   484,   528,   523,     0,   348,
     339,   341,   573,   337,   573,     0,   485,     0,   498,   543,
     495,     0,   499,     0,   509,   529,     0,   512,   537,     0,
     514,   541,   449,     0,   438,   206,   207,   374,   573,     0,
     372,   371,   264,     0,    83,    77,     0,     0,     0,     0,
       0,   586,     0,     0,     0,     0,   394,    61,     0,   400,
       0,     0,   393,    59,   389,    54,     0,     0,   586,   311,
       0,     0,   400,   314,   546,   517,     0,     0,   319,   410,
     411,   586,   412,     0,   586,   333,     0,     0,   331,     0,
       0,   400,     0,     0,     0,     0,     0,   400,     0,   118,
     445,   303,     0,     0,   261,   249,   586,    11,   277,   234,
     400,   573,     0,   346,     0,   525,     0,   376,     0,     0,
     294,     0,   492,   586,   542,   501,   527,   502,   502,   502,
     534,   502,   523,   502,   429,   370,   573,     0,   488,   489,
     586,   586,   355,     0,   531,   355,   355,   353,     0,     0,
     262,    75,    42,   237,   558,   559,   573,   558,   559,     0,
       0,    39,   194,    38,   195,    62,   576,     0,    36,   192,
      37,   193,    60,   390,   391,     0,     0,     0,     0,   481,
     309,   573,     0,   483,   517,     0,     0,   414,   334,     0,
      12,   416,     0,   295,     0,   296,     0,     0,   306,   260,
     586,   244,   338,   349,     0,   344,   340,   382,     0,     0,
       0,   497,     0,   505,     0,   507,     0,   513,     0,   510,
     515,     0,   373,     0,   487,     0,   351,   352,   361,   363,
     530,     0,   366,     0,   368,   387,   263,   400,   230,   229,
      35,   191,   404,   402,     0,     0,   482,   317,     0,     0,
     413,     0,    96,   103,     0,   415,     0,   299,   302,     0,
     406,   407,   405,     0,   347,     0,   342,   380,   573,   378,
     381,   385,   384,   502,   502,   502,   502,   375,   282,     0,
     490,   586,   354,   355,   355,   355,   532,   355,   355,    56,
     312,     0,   102,     0,   586,     0,   586,   586,     0,   345,
       0,     0,   377,   506,     0,   503,   508,   511,   562,   281,
     350,     0,   358,     0,   360,     0,   367,     0,   364,   369,
     320,    99,   101,   573,   558,   559,   408,   332,   307,   343,
     379,   502,   355,   355,   355,   355,    97,   504,   359,     0,
     356,   362,   365,   355,   357
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    65,    66,    67,   274,   408,   409,   283,
     284,   460,    69,   554,    70,   204,    71,    72,   613,   748,
      73,    74,   285,    75,    76,    77,   485,    78,   205,   114,
     115,   230,   231,   232,   649,   591,   198,    80,   290,   558,
     592,   264,   450,   451,   265,   266,   255,   443,   449,   452,
     548,    81,   201,   288,   676,   289,   304,   695,   211,   775,
     212,   776,   648,   939,   616,   614,   858,   402,   404,   625,
     626,   864,   277,   412,   640,   767,   768,   217,   690,   691,
     692,   962,   909,   816,   719,   720,   796,   948,   949,   503,
     800,   342,   543,    83,    84,   390,   606,   605,   435,   942,
     629,   761,   866,   870,    85,    86,   317,   318,   524,    87,
      88,    89,   657,   240,   241,   242,   430,    90,    91,    92,
     311,    93,    94,   207,   208,    97,   209,   398,   615,   756,
     504,   505,   819,   820,   506,   507,   508,   805,   704,   757,
     511,   512,   513,   514,   515,   516,   824,   825,   517,   518,
     519,   520,   521,   698,   200,   403,   295,   453,   259,   120,
     620,   594,   407,   401,   382,   243,   457,   458,   739,   476,
     413,   272,   246,   287
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -785
static const yytype_int16 yypact[] =
{
    -785,   132,  1327,  -785,  7270,  9225,  9558,  5373,  -785,  8880,
    8880,  -785,  -785,  9336,  6774,  5114,  7730,  7730,  -785,  -785,
    7730,  2910,  6132,  -785,  -785,  -785,  -785,   223,  6774,  -785,
       5,  -785,  -785,  -785,  5511,  5626,  -785,  -785,  5741,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  8995,  8995,   107,  4391,
      28,  7960,  8190,  7048,  -785,  6500,  1190,  1337,  1404,  1420,
    1005,  -785,   252,  9110,  8995,  -785,   474,  -785,   905,  -785,
     482,  -785,  -785,   124,   154,  -785,    73,  9447,  -785,   171,
   11552,   130,   272,   125,   108,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,   337,   209,  -785,   370,    64,
    -785,  -785,  -785,  -785,  -785,   179,   179,   244,   158,   341,
    8880,   168,  4507,   507,  -785,   186,  -785,   494,  -785,  -785,
      64,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,    59,
     193,   195,   197,  -785,  -785,  -785,  -785,  -785,  -785,   208,
     212,   227,   228,  -785,   232,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
     238,  3587,   293,   482,   655,   237,   842,    41,   298,    56,
     655,  8880,  8880,   345,  -785,  -785,   918,   377,    53,    97,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  6637,
    -785,  -785,   110,  -785,  -785,  -785,  -785,  -785,  -785,   474,
    -785,   497,  -785,   428,  -785,  -785,   474,  2759,  8995,  8995,
    8995,  8995,  -785, 11531,  -785,  -785,   327,   400,   332,  -785,
    -785,  -785,  7730,  -785,  -785,  -785,  7730,  -785,  -785,  -785,
    5230,  8880,  -785,  -785,   347,  4623,  -785,   939,   413,   466,
    7500,  4391,   362,   474,   905,   368,   403,  -785,  7500,   368,
     392,   -17,     4,  -785, 11531,   406,     4,  -785,   490,  9669,
     433,   943,   947,   977,  1150,  -785,  -785,  -785,  -785,  1457,
    -785,  -785,  -785,  -785,  -785,  -785,   561,  1227,  -785,  -785,
    1477,  -785,  1511,  -785,  1514,  -785,  1146,   503,   514,  -785,
    -785,  -785,  -785,  4882,  8880,  8880,  8880,  8880,  7500,  8880,
    8880,  -785,  -785,  8305,  -785,  4391,  7159,   454,  8305,  8995,
    8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,
    8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,  8995,
    8995,  8995,  8995,  8995,  8995,  9968,  -785,  7730,  -785, 10051,
    -785,  -785, 11213,  -785,  -785,  -785,  9110,  9110,  -785,   500,
    -785,   482,  -785,  1004,  -785,  -785,  -785,  -785,  -785, 10134,
    7730, 10217,  3587,  8880,  -785,  -785,  -785,  -785,   585,   602,
      67,  -785,  3730,   604,  8995, 10300,  7730, 10383,  8995,  8995,
    4016,   732,   732,   104, 10466,  7730, 10549,  -785,   565,  -785,
    4623,   428,  -785,  -785,  8420,   620,  -785,  8995, 11552, 11552,
   11552,  8995,   561,  -785,  7845,  -785,  8995,  7615,  -785,   543,
     368,  -785,   513,   519,  -785,  -785,   134,   504,  -785,  -785,
    6774,  4132,   516, 10300, 10383,  8995,   905,   368,  -785,  -785,
    4998,   521,   905,  -785,  -785,  8075,  -785,  -785,  -785,  -785,
    -785,  -785,  1004,    73,  9669,  -785,  9669, 10632,  7730, 10715,
      76,  -785,  -785,  -785,  -785,  -785,  1586,  -785,  -785,  -785,
    -785,  -785,  1217,  -785,  8995,  -785,   522,   617,   534,  -785,
    -785,  -785,  -785,  -785,   541,  8995,  -785,   545,   636,   552,
     652,  -785,  -785,  1518,  4623,   561,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  8995,  8995,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,    47,  8995,  -785, 11359,   327,  -785,   368,
    9669,   573,  -785,  -785,  -785,   670,   597,  9888,  -785,  -785,
    1013,   215,   413,  9988,  9988,  9988,  9988,  1281,  1281, 10071,
    2591,  9988,  9988, 11612, 11612,   811,   811, 11299,  1281,  1281,
    1378,  1378,   886,   790,   790,   413,   413,   413,  3043,  6247,
    3309,  6362,  -785,   179,  -785,   368,   532,  -785,   557,  -785,
    -785,  6132,  -785,  -785,  2558,    47,    47,  -785, 11282,  -785,
    -785,  -785,  -785,  -785,   474,  8880,  3587,   749,   754,  -785,
     179,   368,   179,   702,   134,  1182,  6911,  -785,  8535,   701,
    -785,   535,  -785,  5856,  5994,   368,   239,   275,   701,  -785,
    -785,  -785,  -785,    43,    70,   368,   118,   123,  8880,  6774,
     598,   706, 11552,   648,  -785, 11552, 11552,   561,  8995, 11531,
    -785,   332, 11552,  -785,  -785,   639,  7845,  7385,  -785,  -785,
    -785,   600,  -785,  -785,    48,   905,   368,     4,   454,  -785,
      57,   754,   368,    71,   398,  -785,  -785,  -785,  1586,   873,
    -785,   595,   368,  -785,   368,    78, 11552,   319,  -785,  -785,
    -785,    39,  -785,  1217,  -785, 11552,  1217,  -785,  -785,   669,
    -785,  -785,  -785,   601,  -785,   413,   413,  -785,  1116,  3587,
    -785,  -785, 11377,  8650,  -785,  -785,  9669,  7500,  9110,  8995,
   10798,  7730, 10881,    60,  9110,  9110,  -785,   500,   605,   612,
    9110,  9110,  -785,   500,   108,   124,  3587,  4623,    47,  -785,
     474,   727,  -785,  -785,  -785,  1217,  3587,   474,  -785, 11359,
    -785,   665,  -785,  4275,   729,  -785,  8880,   743,  -785,  8995,
    8995,   297,  8995,  8995,   751,  4766,  4766,   126,   732,  -785,
    -785,  -785,  8765,  3873, 11552,  -785,   634,  -785,  -785,  -785,
     568,   368,  1597,   647,  1605,  -785,   649,   654,  3587,  4623,
    -785,   720,  -785,   534,  -785,  -785,  -785,   664,   667,   672,
    -785,   678,   720,   672,  -785,  -785,   368,   685,  9780,  -785,
     681,   534,   682,  9780,  -785,   689,   694,  -785,   788,  8995,
   11445,  -785,  -785, 11552,  3176,  3442,   368,   314,   316,  8995,
    8995,  -785,  -785,  -785,  -785,  -785,  -785,  9110,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,   803,   697,  4623,  3587,  -785,
    -785,   368,   823,  -785,  1182,  9891,   655,  -785,  -785,  4766,
    -785,  -785,   655,  -785,  8995,  -785,   828,   830,  -785, 11552,
     248,  7385,  -785,   715,  1597,   880,  -785,  -785,  1049,   843,
     723,  -785,  1217,  -785,   669,  -785,   669,  -785,   669,  -785,
    -785,   740,  -785,   818,  1036,   319,  -785,  -785,  1217,  -785,
    1036,  1217,  -785,   669,  -785,  -785, 11463,   359, 11552, 11552,
    -785,  -785,  -785,  -785,   738,   861,  -785,  -785,  3587,   824,
    -785,  1041,   947,   977,  3587,  -785,  3730,  -785,  -785,  4766,
    -785,  -785,  -785,  1597,   715,  1597,   757,  -785,   176,  -785,
    -785,  -785,  -785,   672,   763,   672,   672,  -785,  -785, 10964,
    -785,   534,  -785,   764,   766,   768,  -785,   772,   768,  -785,
    -785,   863,  1004, 11047,  7730, 11130,   602,   535,   872,   715,
    1597,  1049,  -785,  -785,   669,  -785,  -785,  -785,   752,   782,
    -785,  1217,  -785,   669,  -785,   669,  -785,   669,  -785,  -785,
    -785,   749,   754,   368,   680,   719,  -785,  -785,  -785,   715,
    -785,   672,   768,   774,   768,   768,   753,  -785,  -785,   669,
    -785,  -785,  -785,   768,  -785
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -785,  -785,  -785,   462,  -785,    20,  -785,  -325,   254,  -785,
       2,  -785,  -210,  -186,   773,    10,   -36,  -785,  -593,  -785,
      14,   919,  -166,   -23,   -44,  -233,  -432,    -3,  1761,   -71,
     936,    16,   -15,  -785,  -785,     7,  -785,  1249,  -785,  -194,
      -6,   -90,  -328,    93,   -13,  -785,  -374,  -248,  -155,   103,
    -297,    31,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,  -785,
    -785,  -785,   224,  -203,  -403,   -24,  -553,  -785,  -729,  -694,
     266,   243,   -98,  -785,  -494,  -785,  -638,  -785,   -25,  -785,
    -785,   222,  -785,  -785,  -785,   -79,  -785,  -785,  -419,  -785,
      -2,  -785,  -785,  -785,  -785,    22,    45,  -159,  -785,  -785,
    -785,  -785,   673,  -260,  -785,   742,  -785,  -785,  -785,    32,
    -785,  -785,  -785,  1474,  2007,   974,  1442,  -785,  -785,   128,
    -167,   305,    89,  -785,  -785,  -785,    55,  -287,  -428,  -296,
    -764,   211,  -646,   162,  -659,  -645,  -784,    96,   307,  -785,
    -550,  -785,     0,  -497,  -785,  -785,  -785,    12,  -429,   797,
    -334,  -785,  -785,   -81,  -785,    -8,   -27,    24,  -566,  -249,
      79,  1789,    21,     1
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -587
static const yytype_int16 yytable[] =
{
     271,   379,   381,   100,    68,   385,    68,   420,   510,   269,
     448,   702,   651,   273,   197,   263,   268,   509,   663,   642,
     210,   197,   267,   196,   101,   239,   286,   329,   258,   258,
     252,   252,   258,   347,   252,   197,   116,   116,   257,   257,
     394,   593,   257,   479,   116,   601,   233,   481,   604,   595,
     300,   559,   293,   297,   679,   752,   526,   806,   332,   823,
     233,   810,   197,   292,   296,   886,   484,   622,   -99,   771,
     660,   310,   621,   257,   257,   660,   764,   623,   -96,   777,
     817,   593,   -91,   601,   116,   774,   389,   333,   635,   707,
     383,   710,   622,   388,   244,  -101,   -90,   645,   883,   263,
     268,   444,   320,   322,   324,   326,   267,   798,   116,   254,
     260,   746,   747,   261,   388,   467,   790,   839,   725,   256,
     256,   442,  -103,   256,   950,   417,   717,   966,   238,  -102,
     622,  -468,     3,   685,   383,   426,   475,   478,   553,   499,
     275,   478,   447,   -98,   845,   244,   419,  -471,  -100,   238,
     852,   -97,   341,   808,   291,   622,   811,   465,   528,   -96,
     682,   528,   279,   528,   500,   528,   -69,   528,   826,  -278,
     718,   339,   340,  -278,  -103,   -91,   553,   553,   901,   549,
     840,   -91,   526,  -558,  -555,  -102,   -83,   526,   799,   -90,
     944,   -88,   609,   611,   686,   -91,   464,  -558,   -91,   384,
     607,   610,   -91,  -471,   262,   448,   694,   966,   -93,   -90,
    -559,   344,   -90,   380,   886,   509,   -90,   950,  -325,   392,
     489,   421,   422,   393,   609,   611,    82,   376,    82,   117,
     117,   431,  -325,   206,   206,   -95,   197,   216,   663,   206,
     206,   206,   -94,   384,   206,   310,  -463,   810,   429,   823,
     806,   979,   823,   484,   857,   483,   -90,   445,   286,   445,
    -463,   -92,   806,   454,   -89,   714,   763,  -325,   434,   378,
     917,   673,   343,    82,  -325,   469,   252,   301,   258,   238,
     252,  -470,   466,  -472,   257,  -473,  1009,   206,   257,   348,
     472,   751,   660,  -555,   831,  -463,  -475,   666,  -555,   386,
    -474,   301,  -463,   282,   470,   387,   891,   545,   397,   471,
     327,   328,   555,   473,   981,  -476,  -463,   484,   244,   262,
    -467,   238,   286,   906,   907,   436,   405,   940,   619,   772,
     116,   760,   823,  -103,   206,    68,    82,  -470,   509,  -472,
     540,  -473,   954,   551,   536,   537,   538,   539,   410,   806,
     555,   555,  -475,   535,   523,   414,  -474,   -98,   964,   455,
     375,   967,   244,   270,   528,   773,   282,  -556,   238,   256,
     801,  -476,  -463,   600,   376,   878,  -467,   116,   454,   893,
     895,   897,   406,   899,   270,   900,   599,   874,   418,   599,
     495,   252,   723,  -100,   828,   600,   593,   714,   601,   257,
     423,   454,   427,   836,   740,   526,   741,   526,   599,   377,
     630,   600,   436,   785,   252,   -97,   378,   454,   498,   499,
     600,   855,   257,   -92,   599,    82,   454,  -468,   789,  -467,
     252,   862,   -98,   599,  -100,   206,   206,  1016,   257,   252,
     678,  1013,   483,  -467,   500,   670,   547,   257,   434,   847,
     783,   547,   663,   445,   445,   -96,   661,   446,   600,   861,
     387,   941,   100,    68,   990,   442,   257,   197,   509,   257,
     447,   599,   675,   889,   664,   -88,  -556,   -97,  -467,   732,
     668,  -556,   461,   600,   349,  -467,   206,   677,  -103,   454,
     206,   674,   233,   484,   206,   206,   599,   257,   782,    82,
     641,   641,   252,   468,    82,    82,   483,   699,   -95,   699,
     257,   742,    82,   238,   744,   116,   -92,   116,   553,   339,
     340,   -68,   478,   301,   553,   983,   985,   986,   987,   462,
     553,   553,   742,   925,   474,   282,   -92,   653,   785,   -92,
     842,   844,   480,   -92,   721,   482,   849,   851,   841,   436,
     733,   818,   766,   763,   848,   850,   465,    82,   206,   206,
     206,   206,    82,   206,   206,   395,   396,   206,   738,    82,
     301,   486,   560,   724,   533,   842,   844,   509,   849,   851,
     737,   116,   399,  1017,  -102,   534,   234,   736,   743,   235,
     236,   745,   552,   -89,   738,   263,   376,   612,   263,   282,
     627,   206,   267,   971,   -94,   267,   721,   721,   738,   432,
     560,   560,   235,   236,   736,   628,   263,   237,   738,   238,
     632,   526,   740,   267,   206,   750,    82,   206,   522,   762,
     765,   400,   765,   197,   779,   654,    82,   553,   378,   765,
     206,   622,   758,   250,    82,   669,  1003,   741,   754,   206,
     -98,   665,   672,   921,    82,   738,   197,   667,   778,   -83,
     697,   920,   445,   934,   411,   853,   631,   797,   700,   936,
     -90,   411,   701,   234,   638,  -100,   235,   236,   786,   703,
     921,   233,   483,   706,   650,    82,   -89,   708,   257,   257,
     709,   797,   555,   749,    82,   -92,   -98,   804,   555,   843,
     788,   804,   847,   711,   555,   555,   -89,   693,   301,   -89,
     301,   726,   206,   -89,   727,   728,   795,   753,   827,   763,
     812,   781,   491,   492,   493,   494,   600,   912,   914,   832,
     -97,   547,   454,   794,   780,  -100,   787,   814,   818,   599,
     495,   818,   860,   869,   818,   252,   818,   846,    82,   721,
     -89,   234,   803,   257,   235,   236,   865,   116,   873,   639,
     234,   496,   867,   235,   236,   871,   875,   234,   498,   499,
     235,   236,   881,   821,   301,  -562,   872,   685,   713,  -400,
    -243,  -243,   203,   203,  -243,   884,   442,   445,   203,   797,
     887,   237,   -98,   238,   500,   -98,   -98,   888,   237,    99,
     238,    99,   892,   915,   699,   894,    99,    99,   902,   738,
     896,   555,    99,    99,    99,   882,   898,    99,   922,   905,
     908,   699,   699,   -98,   818,   -98,   818,   911,   818,   859,
     818,  -100,   913,   923,  -100,  -100,   863,  -562,   927,   206,
      82,  -400,  -281,   937,   234,   938,    99,   235,   236,   116,
     975,  -562,   818,   943,   116,  -400,  -281,   641,   951,   952,
      99,   349,  -100,   957,  -100,   992,   994,   996,   809,   998,
     999,   813,   206,   958,   969,   237,   970,   238,  1000,   972,
     822,   765,   349,   391,  -562,   926,  -562,  1008,  -400,  -558,
    -400,  -281,  -558,  -562,  -559,   980,   116,  -400,  -281,   693,
     793,   984,   991,   257,   993,   804,   995,    99,   827,    99,
     997,   827,  1019,   827,  1018,  1020,  1021,  1022,   372,   373,
     374,   982,  -559,   671,   687,  1024,   491,   492,   493,   494,
     415,   687,   214,   491,   492,   493,   494,   370,   371,   372,
     373,   374,   121,    82,   376,   334,   335,   336,   337,   338,
     301,    82,   560,  1007,   791,   206,  1010,   349,   560,   206,
     961,   815,   699,   821,   560,   560,   821,   854,   821,   600,
      82,    82,   362,   363,  1006,   454,   738,   630,   765,   416,
      82,   199,   599,   433,   203,   203,   378,    82,   252,   525,
     206,   827,   928,   827,   960,   827,   257,   827,    99,    82,
      82,   856,   802,   693,   963,   693,   424,    82,    99,    99,
     807,   792,   370,   371,   372,   373,   374,   868,   945,   827,
     376,     0,    82,    82,     0,     0,     0,   463,     0,   876,
     877,   487,     0,     0,     0,  -469,     0,   880,     0,     0,
       0,   376,   904,   456,   459,   376,   821,   910,   821,  -469,
     821,     0,   821,   890,   953,   425,   955,     0,     0,    99,
     956,     0,   378,    99,     0,  -271,   325,    99,    99,   313,
     314,   560,    99,   965,   821,   968,   416,    99,    99,  -271,
     488,    82,    82,   378,  -469,    99,     0,   378,     0,   931,
       0,  -469,  -282,    82,     0,   693,   946,     0,     0,     0,
     947,   730,   491,   492,   493,   494,  -282,   203,   203,   203,
     203,   924,   541,   542,  -271,   376,     0,     0,   315,   316,
       0,  -271,     0,   935,   959,     0,     0,     0,     0,   973,
      99,    99,    99,    99,    99,    99,    99,    99,   376,     0,
      99,  -282,    99,   376,     0,    99,  1011,     0,  -282,     0,
     731,     0,    82,  1012,   693,  1014,   693,   378,    82,  1015,
      82,     0,     0,    82,     0,     0,     0,   490,     0,   491,
     492,   493,   494,   400,    99,     0,   624,     0,   974,     0,
     378,  1023,     0,    99,    99,   378,     0,   495,   976,     0,
     977,   693,     0,   978,     0,     0,     0,    99,   206,    99,
      99,   490,     0,   491,   492,   493,   494,   532,   496,    99,
     313,   314,     0,    99,   497,   498,   499,    99,     0,     0,
       0,   495,    99,     0,     0,     0,     0,    99,     0,     0,
       0,     0,     0,   490,     0,   491,   492,   493,   494,     0,
       0,   500,   496,     0,   501,     0,     0,     0,   497,   498,
     499,     0,   312,   495,   313,   314,     0,     0,    99,   315,
     316,   238,     0,     0,     0,   253,   253,    99,   490,   253,
     491,   492,   493,   494,   496,   500,     0,     0,   501,     0,
     497,   498,   499,     0,     0,    99,     0,     0,   495,   527,
     502,   313,   314,     0,     0,   276,   278,     0,     0,     0,
     253,   294,     0,   315,   316,     0,     0,   500,     0,   496,
     501,     0,   330,   331,     0,   497,   498,   499,     0,     0,
       0,    99,   755,     0,     0,     0,     0,  -586,     4,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
     315,   316,   500,     0,     0,   501,    15,     0,    16,    17,
      18,    19,   349,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,   362,   363,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,   203,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
     319,   313,   314,     0,   367,   368,   369,   370,   371,   372,
     373,   374,    99,    99,     0,     0,    48,     0,     0,    49,
      50,   203,    51,    52,     0,    53,     0,     0,    54,     0,
      55,    56,    57,    58,    59,    60,     0,     0,    61,  -586,
       0,     0,  -586,  -586,    98,    99,    98,   119,   119,   349,
     315,   316,     0,     0,     0,   219,     0,     0,    62,    63,
      64,     0,     0,     0,   362,   363,     0,   321,   313,   314,
    -586,     0,  -586,     0,     0,     0,    95,     0,    95,   118,
     118,   118,     0,   323,   313,   314,     0,   218,     0,     0,
       0,    98,     0,     0,     0,   303,     0,   438,   439,   440,
     330,     0,     0,   369,   370,   371,   372,   373,   374,     0,
       0,   253,     0,     0,     0,   253,    99,   315,   316,   303,
     522,   313,   314,    95,    99,    99,     0,   302,    99,     0,
       0,    99,    99,   315,   316,     0,     0,    99,    99,   203,
     529,   313,   314,    99,    99,     0,     0,     0,     0,     0,
       0,   302,     0,    99,    98,     0,     0,     0,     0,     0,
      99,     0,     0,    99,     0,     0,     0,     0,     0,     0,
     315,   316,    99,    99,   530,   313,   314,   531,   313,   314,
      99,   712,   313,   314,     0,     0,    95,     0,     0,     0,
     315,   316,   546,     0,     0,    99,    99,   557,   562,   563,
     564,   565,   566,   567,   568,   569,   570,   571,   572,   573,
     574,   575,   576,   577,   578,   579,   580,   581,   582,   583,
     584,   585,   586,   587,   315,   316,   253,   315,   316,     0,
       0,   315,   316,     0,     0,   608,   608,   687,     0,   491,
     492,   493,   494,    98,    99,     0,     0,     0,   687,   253,
     491,   492,   493,   494,    99,    99,   687,     0,   491,   492,
     493,   494,     0,   608,     0,   253,    99,   608,   608,     0,
       0,     0,     0,     0,   253,    95,     0,     0,   688,     0,
       0,     0,     0,   652,   689,     0,   655,     0,     0,   688,
     656,     0,     0,   659,     0,   662,   294,   688,     0,     0,
       0,     0,     0,   885,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   608,     0,     0,    98,     0,     0,
       0,     0,    98,    98,   659,    99,     0,     0,     0,     0,
      98,    99,     0,    99,     0,     0,    99,   253,     0,     0,
       0,   303,     0,     0,     0,     0,     0,     0,     0,    95,
       0,     0,     0,   696,    95,    95,     0,     0,     0,     0,
       0,     0,    95,    79,   705,    79,     0,     0,     0,     0,
       0,    99,     0,   302,   215,    98,     0,     0,     0,     0,
      98,     0,   715,   716,     0,     0,     0,    98,   303,     0,
     561,     0,     0,   722,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   245,     0,     0,    95,     0,     0,
      79,     0,    95,     0,     0,     0,     0,     0,     0,    95,
     302,     0,     0,     0,     0,     0,     0,     0,   561,   561,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    98,   245,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    98,    79,     0,     0,    95,   759,     0,     0,
       0,     0,     0,     0,     0,     0,    95,     0,     0,     0,
       0,     0,     0,     0,    95,     0,     0,     0,     0,     0,
       0,     0,     0,    98,    95,     0,     0,   784,     0,     0,
       0,     0,    98,     0,     0,   659,   294,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   303,     0,   303,     0,
       0,     0,     0,     0,     0,    95,     0,     0,     0,     0,
       0,     0,     0,     0,    95,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   302,     0,
     302,     0,    79,     0,     0,     0,    98,     0,     0,     0,
       0,     0,   830,     0,     0,     0,     0,   608,   833,     0,
     253,     0,     0,   608,   608,     0,     0,     0,     0,   608,
     608,     0,   303,   245,     0,     0,     0,     0,    95,   245,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    96,
       0,    96,     0,     0,     0,     0,     0,     0,   608,   608,
       0,   608,   608,     0,   302,     0,     0,     0,   245,     0,
       0,   879,     0,     0,     0,   245,    79,     0,     0,     0,
       0,    79,    79,     0,     0,     0,     0,     0,     0,    79,
       0,     0,     0,     0,     0,     0,    96,     0,    98,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   245,     0,     0,     0,     0,     0,   916,     0,
     477,   477,     0,     0,     0,   477,     0,     0,   918,   919,
      95,     0,     0,     0,    79,     0,   608,     0,     0,    79,
       0,     0,     0,     0,     0,     0,    79,     0,     0,   556,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    96,
       0,     0,   245,   608,     0,     0,     0,     0,     0,     0,
     294,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   556,   556,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    98,     0,    79,     0,     0,     0,     0,   303,    98,
     561,     0,     0,    79,     0,     0,   561,     0,     0,     0,
       0,    79,   561,   561,     0,     0,     0,     0,    98,    98,
       0,    79,     0,    95,     0,     0,     0,     0,    98,     0,
     302,    95,     0,     0,     0,    98,     0,     0,    96,     0,
     245,   245,     0,     0,     0,     0,     0,    98,    98,     0,
      95,    95,    79,   253,     0,    98,     0,     0,     0,     0,
      95,    79,     0,     0,     0,     0,     0,    95,     0,     0,
      98,    98,     0,     0,     0,     0,     0,     0,     0,    95,
      95,     0,     0,     0,     0,     0,     0,    95,     0,   245,
     119,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,    95,    95,     0,     0,     0,     0,     0,     0,
       0,     0,    96,     0,     0,    79,     0,    96,    96,   561,
       0,     0,   118,     0,     0,    96,     0,   118,     0,    98,
      98,     0,     0,     0,     0,     0,     0,   933,     0,     0,
       0,    98,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    95,    95,     0,     0,     0,     0,     0,     0,   932,
      96,     0,     0,    95,     0,    96,     0,     0,     0,     0,
       0,     0,    96,     0,     0,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    98,    79,    98,     0,
       0,    98,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    96,    96,     0,     0,     0,     0,     0,
       0,     0,    95,   245,     0,     0,     0,     0,    95,    96,
      95,     0,     0,    95,     0,     0,     0,     0,     0,    96,
       0,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,     0,   245,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   477,     0,    96,     0,
       0,     0,     0,     0,     0,     0,     0,    96,     0,     0,
      79,     0,     0,     0,     0,     0,     0,     0,    79,   556,
       0,     0,     0,     0,     0,   556,     0,     0,     0,     0,
       0,   556,   556,     0,     0,     0,     0,    79,    79,     0,
       0,     0,     0,     0,     0,     0,     0,    79,     0,     0,
       0,     0,     0,     0,    79,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,     0,    79,    79,     0,   245,
       0,     0,     0,     0,    79,     0,   245,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -586,    79,
      79,     0,     0,     0,     0,     0,     0,   245,     0,     0,
       0,  -586,  -586,  -586,  -586,  -586,  -586,     0,  -586,     0,
       0,     0,     0,     0,  -586,  -586,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -586,  -586,     0,  -586,  -586,
    -586,  -586,  -586,     0,     0,     0,     0,     0,   556,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    79,    79,
       0,     0,     0,    96,     0,     0,   930,     0,     0,     0,
      79,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -586,     0,     0,     0,
       0,     0,     0,     0,     0,   245,     0,     0,     0,     0,
    -586,   245,   349,   350,   351,   352,   353,   354,   355,   356,
    -586,   358,   359,  -586,  -586,     0,     0,   362,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    79,
       0,     0,     0,  -586,  -586,    79,     0,    79,   262,  -586,
      79,  -586,  -586,  -586,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,   367,   368,   369,   370,   371,   372,
     373,   374,     0,     0,     0,     0,    96,     0,     0,     0,
       0,     0,     0,     0,    96,    96,     0,     0,     0,     0,
       0,    96,     0,     0,     0,     0,     0,    96,    96,     0,
       0,     0,     0,    96,    96,     0,     0,     0,     0,  -463,
       0,     0,     0,    96,     0,     0,     0,     0,     0,     0,
      96,     0,  -463,  -463,  -463,  -463,  -463,  -463,     0,  -463,
       0,     0,    96,    96,     0,     0,  -463,  -463,     0,     0,
      96,     0,     0,     0,     0,     0,  -463,  -463,     0,  -463,
    -463,  -463,  -463,  -463,     0,    96,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   437,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,
    -463,  -463,  -463,     0,     0,  -463,  -463,  -463,     0,  -463,
    -463,     0,     0,     0,    96,     0,  -463,     0,     0,     0,
       0,  -463,     0,     0,    96,    96,     0,     0,     0,     0,
       0,  -463,     0,     0,  -463,  -463,    96,  -463,  -463,     0,
    -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,  -463,
       0,     0,     0,     0,     0,  -463,  -463,  -463,     0,     0,
    -463,  -463,  -463,  -463,  -463,     0,     0,     0,     0,     0,
    -586,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -586,  -586,  -586,  -586,  -586,  -586,     0,
    -586,     0,     0,     0,     0,    96,     0,  -586,  -586,     0,
       0,    96,     0,    96,     0,     0,    96,  -586,  -586,     0,
    -586,  -586,  -586,  -586,  -586,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,
    -586,  -586,  -586,  -586,     0,     0,  -586,  -586,  -586,     0,
       0,  -586,     0,     0,     0,     0,     0,  -586,     0,     0,
       0,     0,  -586,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -586,     0,     0,  -586,  -586,     0,     0,  -586,
       0,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,
    -586,     0,     0,  -562,     0,     0,  -586,  -586,  -586,     0,
     262,  -586,  -586,  -586,  -586,  -586,  -562,  -562,  -562,     0,
    -562,  -562,     0,  -562,     0,     0,     0,     0,     0,  -562,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -562,  -562,     0,  -562,  -562,  -562,  -562,  -562,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -562,  -562,  -562,  -562,  -562,  -562,
    -562,  -562,  -562,  -562,  -562,  -562,  -562,     0,     0,  -562,
    -562,  -562,     0,   734,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -562,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -562,     0,     0,  -562,  -562,
       0,   -99,  -562,     0,  -562,  -562,  -562,  -562,  -562,  -562,
    -562,  -562,  -562,  -562,     0,     0,  -562,     0,  -562,  -562,
    -562,   -91,     0,     0,  -562,     0,  -562,  -562,  -562,  -562,
    -562,  -562,     0,  -562,  -562,     0,  -562,     0,     0,     0,
       0,     0,  -562,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -562,  -562,     0,  -562,  -562,  -562,  -562,
    -562,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -562,  -562,  -562,
    -562,  -562,  -562,  -562,  -562,  -562,  -562,  -562,  -562,  -562,
       0,     0,  -562,  -562,  -562,     0,   734,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -562,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -562,     0,
       0,  -562,  -562,     0,   -99,  -562,     0,  -562,  -562,  -562,
    -562,  -562,  -562,  -562,  -562,  -562,  -562,     0,     0,  -281,
       0,  -562,  -562,  -562,  -562,     0,     0,  -562,     0,  -562,
    -562,  -562,  -281,  -281,  -281,     0,  -281,  -281,     0,  -281,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -281,  -281,     0,  -281,
    -281,  -281,  -281,  -281,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,
    -281,  -281,  -281,     0,     0,  -281,  -281,  -281,     0,   735,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -281,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -281,     0,     0,  -281,  -281,     0,  -101,  -281,     0,
    -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,  -281,
       0,     0,  -281,     0,     0,  -281,  -281,   -93,     0,     0,
    -281,     0,  -281,  -281,  -281,  -281,  -281,  -281,     0,  -281,
    -281,     0,  -281,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -281,
    -281,     0,  -281,  -281,  -281,  -281,  -281,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -281,  -281,  -281,  -281,  -281,  -281,  -281,
    -281,  -281,  -281,  -281,  -281,  -281,     0,     0,  -281,  -281,
    -281,     0,   735,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -281,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -281,     0,     0,  -281,  -281,     0,
    -101,  -281,     0,  -281,  -281,  -281,  -281,  -281,  -281,  -281,
    -281,  -281,  -281,     0,     0,     0,     0,     0,  -281,  -281,
    -281,     0,     0,  -281,     0,  -281,  -281,  -281,   280,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    -586,  -586,  -586,     0,     0,  -586,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,     0,    54,     0,
      55,    56,    57,    58,    59,    60,     0,     0,    61,  -586,
       0,     0,  -586,  -586,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -586,   280,  -586,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,  -586,     0,  -586,  -586,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
       0,     0,    49,    50,     0,    51,    52,     0,    53,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,  -586,     0,     0,  -586,  -586,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -586,   280,  -586,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,  -586,     0,
       0,  -586,    15,  -586,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,     0,     0,    49,    50,     0,    51,    52,
       0,    53,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,  -586,     0,     0,  -586,  -586,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -586,   280,  -586,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,  -586,     0,     0,  -586,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,     0,     0,    49,    50,
       0,    51,    52,     0,    53,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,  -586,     0,
       0,  -586,  -586,     4,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    62,    63,    64,
       0,    15,     0,    16,    17,    18,    19,     0,     0,  -586,
       0,  -586,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,  -586,     0,     0,  -586,  -586,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,    64,     0,     0,  -586,     0,
       0,     0,     0,     0,     0,  -586,   280,  -586,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,  -586,
    -586,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,     0,     0,    49,    50,     0,
      51,    52,     0,    53,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,  -586,     0,     0,
    -586,  -586,   280,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    62,    63,    64,     0,
      15,     0,    16,    17,    18,    19,     0,     0,  -586,     0,
    -586,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,     0,     0,   281,    50,     0,    51,    52,     0,    53,
       0,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -586,     0,     0,  -586,  -586,   280,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    62,    63,    64,     0,    15,     0,    16,    17,
      18,    19,  -586,     0,  -586,     0,  -586,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,     0,    54,     0,
      55,    56,    57,    58,    59,    60,     0,     0,    61,  -586,
       0,     0,  -586,  -586,   280,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    62,    63,
      64,     0,    15,     0,    16,    17,    18,    19,  -586,     0,
    -586,     0,  -586,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,     0,     0,    49,    50,     0,    51,    52,
       0,    53,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,  -586,     0,     0,  -586,  -586,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,    64,     0,     0,  -586,
       0,     0,     0,     0,     0,     0,  -586,   280,  -586,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,  -586,     0,     0,     0,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,     0,     0,    49,    50,
       0,    51,    52,     0,    53,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,  -586,     0,
       0,  -586,  -586,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    62,    63,    64,
       0,    15,     0,    16,    17,    18,    19,     0,     0,  -586,
       0,  -586,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,   234,     0,     0,   235,   236,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    62,    63,    64,     0,    15,     0,    16,
      17,    18,    19,     0,     0,   237,     0,   238,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,     0,     0,
      49,    50,     0,    51,    52,     0,    53,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
     234,     0,     0,   235,   236,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,    62,
      63,    64,     0,    15,     0,    16,    17,    18,    19,     0,
       0,   237,     0,   238,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   202,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,   234,     0,     0,   235,
     236,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,    62,    63,    64,     0,    15,
       0,    16,    17,    18,    19,     0,     0,   237,     0,   238,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,     0,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   238,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
       0,     0,     0,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,     0,     0,     0,     0,     0,   156,   157,
     158,   159,   160,   161,   162,   163,    36,    37,   164,    39,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,     0,   192,   193,     0,     0,     0,
       0,     0,   194,   195,  -555,  -555,  -555,  -555,  -555,  -555,
    -555,  -555,  -555,     0,     0,     0,     0,     0,     0,     0,
    -555,     0,  -555,  -555,  -555,  -555,     0,  -555,     0,     0,
       0,  -555,  -555,  -555,  -555,  -555,  -555,  -555,     0,     0,
    -555,     0,     0,     0,     0,     0,     0,     0,     0,  -555,
    -555,  -555,  -555,  -555,  -555,  -555,  -555,  -555,     0,  -555,
    -555,  -555,     0,     0,  -555,     0,     0,  -555,  -555,     0,
    -555,  -555,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -555,     0,     0,  -555,  -555,     0,  -555,  -555,     0,  -555,
    -555,  -555,  -555,     0,  -555,  -555,  -555,  -555,  -555,  -555,
       0,     0,  -555,     0,     0,     0,     0,     0,     0,  -557,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,     0,     0,
       0,     0,  -555,  -555,  -555,  -557,  -555,  -557,  -557,  -557,
    -557,  -555,  -557,     0,     0,     0,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,     0,     0,  -557,     0,     0,     0,     0,
       0,     0,     0,     0,  -557,  -557,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,     0,  -557,  -557,  -557,     0,     0,  -557,
       0,     0,  -557,  -557,     0,  -557,  -557,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -557,     0,     0,  -557,  -557,
       0,  -557,  -557,     0,  -557,  -557,  -557,  -557,     0,  -557,
    -557,  -557,  -557,  -557,  -557,     0,     0,  -557,     0,     0,
       0,     0,     0,     0,  -556,  -556,  -556,  -556,  -556,  -556,
    -556,  -556,  -556,     0,     0,     0,     0,  -557,  -557,  -557,
    -556,  -557,  -556,  -556,  -556,  -556,  -557,  -556,     0,     0,
       0,  -556,  -556,  -556,  -556,  -556,  -556,  -556,     0,     0,
    -556,     0,     0,     0,     0,     0,     0,     0,     0,  -556,
    -556,  -556,  -556,  -556,  -556,  -556,  -556,  -556,     0,  -556,
    -556,  -556,     0,     0,  -556,     0,     0,  -556,  -556,     0,
    -556,  -556,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -556,     0,     0,  -556,  -556,     0,  -556,  -556,     0,  -556,
    -556,  -556,  -556,     0,  -556,  -556,  -556,  -556,  -556,  -556,
       0,     0,  -556,     0,     0,     0,     0,     0,     0,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,     0,     0,
       0,     0,  -556,  -556,  -556,  -558,  -556,  -558,  -558,  -558,
    -558,  -556,     0,     0,     0,     0,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,     0,     0,  -558,     0,     0,     0,     0,
       0,     0,     0,     0,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,     0,  -558,  -558,  -558,     0,     0,  -558,
       0,     0,  -558,  -558,     0,  -558,  -558,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -558,   769,     0,  -558,  -558,
       0,  -558,  -558,     0,  -558,  -558,  -558,  -558,     0,  -558,
    -558,  -558,  -558,  -558,  -558,     0,     0,  -558,     0,     0,
       0,     0,     0,     0,   -99,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -558,  -558,  -558,
       0,     0,     0,     0,     0,     0,  -558,  -559,  -559,  -559,
    -559,  -559,  -559,  -559,  -559,  -559,     0,     0,     0,     0,
       0,     0,     0,  -559,     0,  -559,  -559,  -559,  -559,     0,
       0,     0,     0,     0,  -559,  -559,  -559,  -559,  -559,  -559,
    -559,     0,     0,  -559,     0,     0,     0,     0,     0,     0,
       0,     0,  -559,  -559,  -559,  -559,  -559,  -559,  -559,  -559,
    -559,     0,  -559,  -559,  -559,     0,     0,  -559,     0,     0,
    -559,  -559,     0,  -559,  -559,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -559,   770,     0,  -559,  -559,     0,  -559,
    -559,     0,  -559,  -559,  -559,  -559,     0,  -559,  -559,  -559,
    -559,  -559,  -559,     0,     0,  -559,     0,     0,     0,     0,
       0,     0,  -101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -559,  -559,  -559,     0,     0,
       0,     0,     0,     0,  -559,  -251,  -251,  -251,  -251,  -251,
    -251,  -251,  -251,  -251,     0,     0,     0,     0,     0,     0,
       0,  -251,     0,  -251,  -251,  -251,  -251,     0,     0,     0,
       0,     0,  -251,  -251,  -251,  -251,  -251,  -251,  -251,     0,
       0,  -251,     0,     0,     0,     0,     0,     0,     0,     0,
    -251,  -251,  -251,  -251,  -251,  -251,  -251,  -251,  -251,     0,
    -251,  -251,  -251,     0,     0,  -251,     0,     0,  -251,  -251,
       0,  -251,  -251,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -251,     0,     0,  -251,  -251,     0,  -251,  -251,     0,
    -251,  -251,  -251,  -251,     0,  -251,  -251,  -251,  -251,  -251,
    -251,     0,     0,  -251,     0,     0,     0,     0,     0,     0,
    -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,     0,
       0,     0,     0,  -251,  -251,  -251,  -560,     0,  -560,  -560,
    -560,  -560,   262,     0,     0,     0,     0,  -560,  -560,  -560,
    -560,  -560,  -560,  -560,     0,     0,  -560,     0,     0,     0,
       0,     0,     0,     0,     0,  -560,  -560,  -560,  -560,  -560,
    -560,  -560,  -560,  -560,     0,  -560,  -560,  -560,     0,     0,
    -560,     0,     0,  -560,  -560,     0,  -560,  -560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -560,     0,     0,  -560,
    -560,     0,  -560,  -560,     0,  -560,  -560,  -560,  -560,     0,
    -560,  -560,  -560,  -560,  -560,  -560,     0,     0,  -560,     0,
       0,     0,     0,     0,     0,  -561,  -561,  -561,  -561,  -561,
    -561,  -561,  -561,  -561,     0,     0,     0,     0,  -560,  -560,
    -560,  -561,     0,  -561,  -561,  -561,  -561,  -560,     0,     0,
       0,     0,  -561,  -561,  -561,  -561,  -561,  -561,  -561,     0,
       0,  -561,     0,     0,     0,     0,     0,     0,     0,     0,
    -561,  -561,  -561,  -561,  -561,  -561,  -561,  -561,  -561,     0,
    -561,  -561,  -561,     0,     0,  -561,     0,     0,  -561,  -561,
       0,  -561,  -561,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -561,     0,     0,  -561,  -561,     0,  -561,  -561,     0,
    -561,  -561,  -561,  -561,     0,  -561,  -561,  -561,  -561,  -561,
    -561,     0,     0,  -561,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -561,  -561,  -561,     0,     0,     0,     0,
       0,     0,  -561,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,     0,     0,     0,
     146,   147,   148,   220,   221,   222,   223,   153,   154,   155,
       0,     0,     0,     0,     0,   156,   157,   158,   224,   225,
     226,   227,   163,   305,   306,   228,   307,     0,     0,     0,
       0,     0,     0,   308,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
     174,   175,     0,     0,   176,   177,   178,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
       0,     0,     0,     0,     0,     0,     0,   309,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,     0,   192,   193,     0,     0,     0,     0,     0,   194,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,     0,     0,     0,   146,   147,   148,
     220,   221,   222,   223,   153,   154,   155,     0,     0,     0,
       0,     0,   156,   157,   158,   224,   225,   226,   227,   163,
     305,   306,   228,   307,     0,     0,     0,     0,     0,     0,
     308,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,     0,     0,     0,
       0,     0,     0,     0,   428,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
     184,   185,   186,   187,   188,   189,   190,   191,     0,   192,
     193,     0,     0,     0,     0,     0,   194,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,     0,     0,     0,   146,   147,   148,   220,   221,   222,
     223,   153,   154,   155,     0,     0,     0,     0,     0,   156,
     157,   158,   224,   225,   226,   227,   163,     0,     0,   228,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,     0,     0,     0,   229,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,     0,     0,
       0,     0,     0,   194,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,     0,     0,
       0,   146,   147,   148,   220,   221,   222,   223,   153,   154,
     155,     0,     0,     0,     0,     0,   156,   157,   158,   224,
     225,   226,   227,   163,     0,     0,   228,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,     0,   192,   193,     0,     0,     0,     0,     0,
     194,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   298,     0,     0,
     112,    50,     0,    51,    52,     0,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   113,
     102,   103,    18,    19,     0,     0,   299,     0,     0,   104,
     105,   106,    23,    24,    25,    26,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   298,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,     0,
      54,     0,    55,    56,    57,    58,    59,    60,     0,     0,
      61,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,     0,     0,     0,    15,
     113,    16,    17,    18,    19,     0,     0,   550,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
       0,     0,    49,    50,     0,    51,    52,     0,    53,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,    63,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   247,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,     0,   249,  -256,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,   234,     0,     0,
     235,   236,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,    62,   251,    64,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
       0,     0,    49,    50,     0,    51,    52,     0,    53,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,    63,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   247,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,     0,   249,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,   234,     0,     0,
     235,   236,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   247,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,   248,   249,
     250,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   247,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,   658,   249,   250,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   247,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,   248,   249,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   247,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,   658,   249,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   247,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,     0,   249,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,   544,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,   248,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,   544,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,   829,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,   658,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,     0,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,    63,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    62,   251,    64,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   112,    50,     0,    51,    52,     0,     0,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   251,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   108,    35,    36,    37,
     109,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,   111,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   113,   102,   103,    18,
      19,     0,     0,     0,     0,     0,   104,   105,   106,    23,
      24,    25,    26,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   213,     0,     0,    49,    50,
       0,    51,    52,     0,    53,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   113,   102,   103,
      18,    19,     0,     0,     0,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   298,     0,     0,   345,
      50,     0,    51,    52,     0,   346,     0,     0,    54,     0,
      55,    56,    57,    58,    59,    60,     0,     0,    61,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   113,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   108,
      35,    36,    37,   109,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
     112,    50,     0,    51,    52,     0,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   113,
     102,   103,    18,    19,     0,     0,     0,     0,     0,   104,
     105,   106,    23,    24,    25,    26,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   298,     0,
       0,   345,    50,     0,    51,    52,     0,     0,     0,     0,
      54,     0,    55,    56,    57,    58,    59,    60,     0,     0,
      61,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     113,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   903,
       0,     0,   112,    50,     0,    51,    52,     0,     0,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,   113,   102,   103,    18,    19,     0,     0,     0,     0,
       0,   104,   105,   106,    23,    24,    25,    26,     0,     0,
     107,     0,   729,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,   359,
     360,   361,     0,     0,   362,   363,     0,     0,     0,     0,
     929,     0,     0,   112,    50,     0,    51,    52,     0,     0,
       0,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,     0,     0,     0,     0,   364,     0,   365,
     366,   367,   368,   369,   370,   371,   372,   373,   374,   588,
     589,     0,   113,   590,     0,     0,  -258,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,   349,
    -587,  -587,  -587,  -587,   354,   355,   180,   181,  -587,  -587,
       0,     0,     0,     0,   362,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   191,     0,
     192,   193,   596,   597,     0,     0,   598,   194,   262,   365,
     366,   367,   368,   369,   370,   371,   372,   373,   374,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,   349,   350,   351,   352,   353,   354,   355,   180,
     181,   358,   359,     0,     0,     0,     0,   362,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,     0,   192,   193,   617,   589,     0,     0,   618,
     194,   262,   365,   366,   367,   368,   369,   370,   371,   372,
     373,   374,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,   602,   597,
       0,     0,   603,   194,   262,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
     184,   185,   186,   187,   188,   189,   190,   191,     0,   192,
     193,   633,   589,     0,     0,   634,   194,   262,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
     174,   175,     0,     0,   176,   177,   178,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,     0,   192,   193,   636,   597,     0,     0,   637,   194,
     262,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,     0,   192,   193,   643,   589,     0,
       0,   644,   194,   262,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,   174,   175,     0,     0,
     176,   177,   178,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,     0,   192,   193,
     646,   597,     0,     0,   647,   194,   262,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
       0,   192,   193,   680,   589,     0,     0,   681,   194,   262,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,     0,     0,   176,   177,   178,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,   183,   184,   185,   186,   187,   188,
     189,   190,   191,     0,   192,   193,   683,   597,     0,     0,
     684,   194,   262,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,     0,   192,   193,   834,
     589,     0,     0,   835,   194,   262,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   191,     0,
     192,   193,   837,   597,     0,     0,   838,   194,   262,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,     0,   192,   193,   988,   589,     0,     0,   989,
     194,   262,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,  1001,   589,
       0,     0,  1002,   194,   262,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
     184,   185,   186,   187,   188,   189,   190,   191,     0,   192,
     193,  1004,   597,     0,     0,  1005,   194,   262,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
     174,   175,     0,     0,   176,   177,   178,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,     0,   192,   193,   602,   597,     0,     0,   603,   194,
     262,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   729,     0,     0,     0,
       0,     0,     0,     0,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,     0,   192,   193,     0,     0,     0,
       0,     0,   194,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,   360,   361,     0,     0,   362,   363,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,     0,     0,   362,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   374,     0,     0,     0,     0,     0,   364,     0,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,     0,   238,   362,   363,     0,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
     361,     0,     0,   362,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
       0,     0,     0,     0,     0,     0,   364,  -258,   365,   366,
     367,   368,   369,   370,   371,   372,   373,   374,     0,     0,
       0,     0,     0,     0,     0,  -259,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,   360,   361,     0,
       0,   362,   363,     0,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,     0,     0,   362,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,     0,     0,     0,     0,
       0,     0,   364,  -260,   365,   366,   367,   368,   369,   370,
     371,   372,   373,   374,     0,     0,     0,     0,     0,     0,
       0,  -261,   349,   350,   351,   352,   353,   354,   355,   356,
     357,   358,   359,   360,   361,     0,     0,   362,   363,     0,
       0,     0,   441,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,   360,   361,     0,     0,   362,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,   365,   366,   367,   368,   369,   370,   371,   372,
     373,   374,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   374,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,  -587,  -587,     0,     0,   362,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   374
};

static const yytype_int16 yycheck[] =
{
      27,    82,    83,     2,     2,    84,     4,   210,   304,    22,
     258,   508,   431,    28,     7,    21,    22,   304,   447,   422,
      10,    14,    21,     7,     4,    15,    49,    63,    16,    17,
      16,    17,    20,    77,    20,    28,     5,     6,    16,    17,
     111,   375,    20,   292,    13,   379,    14,   296,   382,   377,
      53,   348,    51,    52,   486,   621,   316,   703,    66,   718,
      28,   706,    55,    51,    52,   794,   299,   401,    25,   635,
     444,    55,   400,    51,    52,   449,   629,   402,    25,   645,
     718,   415,    25,   417,    53,   638,    99,    66,   416,   517,
      26,   519,   426,    99,    15,    25,    25,   425,   792,   105,
     106,   256,    57,    58,    59,    60,   105,    29,    77,    16,
      17,   605,   606,    20,   120,   281,   682,    57,   550,    16,
      17,   138,    25,    20,   888,   206,    79,   911,   145,    25,
     464,    90,     0,    57,    26,   216,   291,   292,   348,   100,
     135,   296,   138,    25,   737,    66,    90,    88,    25,   145,
     743,    25,    28,   703,    51,   489,   706,    90,   317,   118,
     488,   320,    55,   322,   125,   324,   118,   326,   718,   141,
     123,    37,    38,   145,   118,   118,   386,   387,   816,   345,
     120,   138,   442,   140,    26,   118,   138,   447,   110,   118,
     884,   138,   386,   387,   118,   138,   277,   140,   141,   135,
     386,   387,   145,   144,   140,   453,   502,   991,   138,   138,
     140,   138,   141,    88,   943,   502,   145,   981,    88,    51,
     301,   211,   212,    55,   418,   419,     2,   102,     4,     5,
       6,   239,   102,     9,    10,   138,   229,    13,   667,    15,
      16,    17,   138,   135,    20,   229,    88,   892,   138,   908,
     896,   945,   911,   486,   748,   299,   138,   256,   281,   258,
     102,   138,   908,   262,   138,   525,    18,   137,    20,   144,
     836,   465,   118,    49,   144,   283,   262,    53,   266,   145,
     266,    88,   280,    88,   262,    88,   980,    63,   266,   118,
     288,   616,   666,   135,   726,   137,    88,   452,   140,    90,
      88,    77,   144,    49,   283,    90,   803,   343,   122,   285,
      58,    59,   348,   289,   138,    88,    88,   550,   239,   140,
      88,   145,   345,   820,   821,   246,    88,   880,   399,    90,
     299,   628,   991,   118,   110,   333,   112,   144,   625,   144,
     338,   144,   892,   346,   334,   335,   336,   337,    55,   995,
     386,   387,   144,   333,   309,   118,   144,   118,   908,   266,
      88,   911,   283,   140,   523,    90,   112,    26,   145,   266,
      51,   144,   144,   379,   102,   778,   144,   346,   377,   807,
     808,   809,   144,   811,   140,   813,   379,    90,    90,   382,
      71,   377,   547,   118,   719,   401,   730,   657,   732,   377,
      55,   400,    25,   731,    90,   665,    90,   667,   401,   137,
     409,   417,   333,   661,   400,   118,   144,   416,    99,   100,
     426,   746,   400,    25,   417,   201,   425,    90,   677,    88,
     416,   756,   118,   426,   118,   211,   212,  1003,   416,   425,
     484,   991,   486,   102,   125,   460,   343,   425,    20,    90,
     653,   348,   881,   452,   453,   118,   444,    57,   464,   755,
      90,   880,   461,   461,   961,   138,   444,   460,   755,   447,
     138,   464,   470,   798,   450,   138,   135,   118,   137,   560,
     456,   140,   135,   489,    71,   144,   262,   475,   118,   488,
     266,   467,   460,   726,   270,   271,   489,   475,   653,   275,
     421,   422,   488,   141,   280,   281,   550,   506,   138,   508,
     488,   601,   288,   145,   604,   484,   118,   486,   728,    37,
      38,   118,   677,   299,   734,   953,   954,   955,   956,   275,
     740,   741,   622,   858,   142,   281,   138,   434,   786,   141,
     734,   735,   136,   145,   543,    55,   740,   741,   734,   470,
     577,   718,    17,    18,   740,   741,    90,   333,   334,   335,
     336,   337,   338,   339,   340,    58,    59,   343,   595,   345,
     346,   138,   348,   549,    71,   769,   770,   864,   772,   773,
     593,   550,    88,  1011,   118,    71,   112,   593,   601,   115,
     116,   604,   138,    25,   621,   601,   102,    97,   604,   345,
      15,   377,   601,   928,   138,   604,   605,   606,   635,   112,
     386,   387,   115,   116,   620,    13,   622,   143,   645,   145,
      16,   881,    90,   622,   400,   615,   402,   403,    63,   628,
     629,   137,   631,   626,   649,    15,   412,   847,   144,   638,
     416,   975,   626,   100,   420,   141,   974,    90,   624,   425,
     118,   138,   136,   847,   430,   682,   649,   138,   648,   138,
     138,   847,   661,   866,    16,   744,   412,   694,    51,   872,
     138,    16,   138,   112,   420,   118,   115,   116,   666,   138,
     874,   649,   726,   138,   430,   461,   118,    51,   666,   667,
     138,   718,   728,   614,   470,   138,    16,   697,   734,   735,
     676,   701,    90,    51,   740,   741,   138,   496,   484,   141,
     486,   138,   488,   145,    44,   118,   692,    15,   718,    18,
      51,    15,    53,    54,    55,    56,   732,   825,   826,   727,
     118,   628,   731,   138,   136,    16,   136,   136,   905,   732,
      71,   908,    15,    14,   911,   731,   913,   142,   524,   748,
     138,   112,   697,   731,   115,   116,    91,   726,    15,    27,
     112,    92,   761,   115,   116,   764,    15,   112,    99,   100,
     115,   116,   138,   718,   550,    26,   766,    57,   524,    26,
     141,   142,     9,    10,   145,   138,   138,   786,    15,   816,
     141,   143,   112,   145,   125,   115,   116,   143,   143,     2,
     145,     4,   138,    15,   803,   138,     9,    10,   123,   836,
     138,   847,    15,    16,    17,   791,   138,    20,    15,   138,
     138,   820,   821,   143,   991,   145,   993,   138,   995,   750,
     997,   112,   138,   136,   115,   116,   757,    88,    15,   615,
     616,    88,    88,    15,   112,    15,    49,   115,   116,   818,
     931,   102,  1019,   138,   823,   102,   102,   778,    15,   136,
      63,    71,   143,   123,   145,   963,   964,   965,   706,   967,
     968,   709,   648,    55,   136,   143,    15,   145,    15,    55,
     718,   880,    71,   110,   135,   861,   137,    15,   135,   140,
     137,   137,   140,   144,   140,   138,   865,   144,   144,   688,
     689,   138,   138,   881,   138,   905,   138,   110,   908,   112,
     138,   911,   138,   913,  1012,  1013,  1014,  1015,   128,   129,
     130,   948,   140,   461,    51,  1023,    53,    54,    55,    56,
      88,    51,    13,    53,    54,    55,    56,   126,   127,   128,
     129,   130,     6,   719,   102,    40,    41,    42,    43,    44,
     726,   727,   728,   977,   688,   731,   981,    71,   734,   735,
     905,   718,   961,   908,   740,   741,   911,   745,   913,   975,
     746,   747,    86,    87,   976,   974,  1003,   976,   977,   137,
     756,     7,   975,   241,   211,   212,   144,   763,   974,   316,
     766,   991,   864,   993,   905,   995,   974,   997,   201,   775,
     776,   747,   697,   792,   908,   794,    88,   783,   211,   212,
     703,   138,   126,   127,   128,   129,   130,   763,   138,  1019,
     102,    -1,   798,   799,    -1,    -1,    -1,    88,    -1,   775,
     776,    88,    -1,    -1,    -1,    88,    -1,   783,    -1,    -1,
      -1,   102,   818,   270,   271,   102,   991,   823,   993,   102,
     995,    -1,   997,   799,   892,   137,   894,    -1,    -1,   262,
     898,    -1,   144,   266,    -1,    88,    61,   270,   271,    64,
      65,   847,   275,   911,  1019,   913,   137,   280,   281,   102,
     137,   857,   858,   144,   137,   288,    -1,   144,    -1,   865,
      -1,   144,    88,   869,    -1,   884,   885,    -1,    -1,    -1,
      51,    88,    53,    54,    55,    56,   102,   334,   335,   336,
     337,   857,   339,   340,   137,   102,    -1,    -1,   113,   114,
      -1,   144,    -1,   869,    88,    -1,    -1,    -1,    -1,    88,
     333,   334,   335,   336,   337,   338,   339,   340,   102,    -1,
     343,   137,   345,   102,    -1,   348,   984,    -1,   144,    -1,
     137,    -1,   928,   991,   943,   993,   945,   144,   934,   997,
     936,    -1,    -1,   939,    -1,    -1,    -1,    51,    -1,    53,
      54,    55,    56,   137,   377,    -1,   403,    -1,   137,    -1,
     144,  1019,    -1,   386,   387,   144,    -1,    71,   934,    -1,
     936,   980,    -1,   939,    -1,    -1,    -1,   400,   974,   402,
     403,    51,    -1,    53,    54,    55,    56,    61,    92,   412,
      64,    65,    -1,   416,    98,    99,   100,   420,    -1,    -1,
      -1,    71,   425,    -1,    -1,    -1,    -1,   430,    -1,    -1,
      -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,
      -1,   125,    92,    -1,   128,    -1,    -1,    -1,    98,    99,
     100,    -1,    62,    71,    64,    65,    -1,    -1,   461,   113,
     114,   145,    -1,    -1,    -1,    16,    17,   470,    51,    20,
      53,    54,    55,    56,    92,   125,    -1,    -1,   128,    -1,
      98,    99,   100,    -1,    -1,   488,    -1,    -1,    71,    62,
     140,    64,    65,    -1,    -1,    46,    47,    -1,    -1,    -1,
      51,    52,    -1,   113,   114,    -1,    -1,   125,    -1,    92,
     128,    -1,    63,    64,    -1,    98,    99,   100,    -1,    -1,
      -1,   524,   140,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     113,   114,   125,    -1,    -1,   128,    19,    -1,    21,    22,
      23,    24,    71,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    86,    87,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,   615,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      63,    64,    65,    -1,   123,   124,   125,   126,   127,   128,
     129,   130,   615,   616,    -1,    -1,    89,    -1,    -1,    92,
      93,   648,    95,    96,    -1,    98,    -1,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,   112,
      -1,    -1,   115,   116,     2,   648,     4,     5,     6,    71,
     113,   114,    -1,    -1,    -1,    13,    -1,    -1,   131,   132,
     133,    -1,    -1,    -1,    86,    87,    -1,    63,    64,    65,
     143,    -1,   145,    -1,    -1,    -1,     2,    -1,     4,     5,
       6,     7,    -1,    63,    64,    65,    -1,    13,    -1,    -1,
      -1,    49,    -1,    -1,    -1,    53,    -1,   248,   249,   250,
     251,    -1,    -1,   125,   126,   127,   128,   129,   130,    -1,
      -1,   262,    -1,    -1,    -1,   266,   719,   113,   114,    77,
      63,    64,    65,    49,   727,   728,    -1,    53,   731,    -1,
      -1,   734,   735,   113,   114,    -1,    -1,   740,   741,   766,
      63,    64,    65,   746,   747,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    -1,   756,   112,    -1,    -1,    -1,    -1,    -1,
     763,    -1,    -1,   766,    -1,    -1,    -1,    -1,    -1,    -1,
     113,   114,   775,   776,    63,    64,    65,    63,    64,    65,
     783,    63,    64,    65,    -1,    -1,   112,    -1,    -1,    -1,
     113,   114,   343,    -1,    -1,   798,   799,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
     361,   362,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,   373,   374,   113,   114,   377,   113,   114,    -1,
      -1,   113,   114,    -1,    -1,   386,   387,    51,    -1,    53,
      54,    55,    56,   201,   847,    -1,    -1,    -1,    51,   400,
      53,    54,    55,    56,   857,   858,    51,    -1,    53,    54,
      55,    56,    -1,   414,    -1,   416,   869,   418,   419,    -1,
      -1,    -1,    -1,    -1,   425,   201,    -1,    -1,    92,    -1,
      -1,    -1,    -1,   434,    98,    -1,   437,    -1,    -1,    92,
     441,    -1,    -1,   444,    -1,   446,   447,    92,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   465,    -1,    -1,   275,    -1,    -1,
      -1,    -1,   280,   281,   475,   928,    -1,    -1,    -1,    -1,
     288,   934,    -1,   936,    -1,    -1,   939,   488,    -1,    -1,
      -1,   299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   275,
      -1,    -1,    -1,   504,   280,   281,    -1,    -1,    -1,    -1,
      -1,    -1,   288,     2,   515,     4,    -1,    -1,    -1,    -1,
      -1,   974,    -1,   299,    13,   333,    -1,    -1,    -1,    -1,
     338,    -1,   533,   534,    -1,    -1,    -1,   345,   346,    -1,
     348,    -1,    -1,   544,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    -1,    -1,   333,    -1,    -1,
      49,    -1,   338,    -1,    -1,    -1,    -1,    -1,    -1,   345,
     346,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   386,   387,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   412,    66,    -1,    -1,    -1,    -1,
      -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   430,   112,    -1,    -1,   402,   628,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   412,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   461,   430,    -1,    -1,   658,    -1,    -1,
      -1,    -1,   470,    -1,    -1,   666,   667,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,   486,    -1,
      -1,    -1,    -1,    -1,    -1,   461,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,
     486,    -1,   201,    -1,    -1,    -1,   524,    -1,    -1,    -1,
      -1,    -1,   723,    -1,    -1,    -1,    -1,   728,   729,    -1,
     731,    -1,    -1,   734,   735,    -1,    -1,    -1,    -1,   740,
     741,    -1,   550,   204,    -1,    -1,    -1,    -1,   524,   210,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     2,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   769,   770,
      -1,   772,   773,    -1,   550,    -1,    -1,    -1,   239,    -1,
      -1,   782,    -1,    -1,    -1,   246,   275,    -1,    -1,    -1,
      -1,   280,   281,    -1,    -1,    -1,    -1,    -1,    -1,   288,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,   616,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   283,    -1,    -1,    -1,    -1,    -1,   829,    -1,
     291,   292,    -1,    -1,    -1,   296,    -1,    -1,   839,   840,
     616,    -1,    -1,    -1,   333,    -1,   847,    -1,    -1,   338,
      -1,    -1,    -1,    -1,    -1,    -1,   345,    -1,    -1,   348,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   333,   874,    -1,    -1,    -1,    -1,    -1,    -1,
     881,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   386,   387,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   719,    -1,   402,    -1,    -1,    -1,    -1,   726,   727,
     728,    -1,    -1,   412,    -1,    -1,   734,    -1,    -1,    -1,
      -1,   420,   740,   741,    -1,    -1,    -1,    -1,   746,   747,
      -1,   430,    -1,   719,    -1,    -1,    -1,    -1,   756,    -1,
     726,   727,    -1,    -1,    -1,   763,    -1,    -1,   201,    -1,
     421,   422,    -1,    -1,    -1,    -1,    -1,   775,   776,    -1,
     746,   747,   461,   974,    -1,   783,    -1,    -1,    -1,    -1,
     756,   470,    -1,    -1,    -1,    -1,    -1,   763,    -1,    -1,
     798,   799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   775,
     776,    -1,    -1,    -1,    -1,    -1,    -1,   783,    -1,   470,
     818,    -1,    -1,    -1,    -1,   823,    -1,    -1,    -1,    -1,
      -1,    -1,   798,   799,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   275,    -1,    -1,   524,    -1,   280,   281,   847,
      -1,    -1,   818,    -1,    -1,   288,    -1,   823,    -1,   857,
     858,    -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,    -1,
      -1,   869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   857,   858,    -1,    -1,    -1,    -1,    -1,    -1,   865,
     333,    -1,    -1,   869,    -1,   338,    -1,    -1,    -1,    -1,
      -1,    -1,   345,    -1,    -1,   348,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     928,    -1,    -1,    -1,    -1,    -1,   934,   616,   936,    -1,
      -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   386,   387,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   928,   614,    -1,    -1,    -1,    -1,   934,   402,
     936,    -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,   412,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   420,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   430,    -1,    -1,
      -1,    -1,   653,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,   461,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   470,    -1,    -1,
     719,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   727,   728,
      -1,    -1,    -1,    -1,    -1,   734,    -1,    -1,    -1,    -1,
      -1,   740,   741,    -1,    -1,    -1,    -1,   746,   747,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,
      -1,    -1,    -1,    -1,   763,    -1,    -1,    -1,    -1,    -1,
      -1,   524,    -1,    -1,    -1,    -1,   775,   776,    -1,   750,
      -1,    -1,    -1,    -1,   783,    -1,   757,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,   798,
     799,    -1,    -1,    -1,    -1,    -1,    -1,   778,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    26,    27,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,   847,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   857,   858,
      -1,    -1,    -1,   616,    -1,    -1,   865,    -1,    -1,    -1,
     869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   866,    -1,    -1,    -1,    -1,
     102,   872,    71,    72,    73,    74,    75,    76,    77,    78,
     112,    80,    81,   115,   116,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   928,
      -1,    -1,    -1,   135,   136,   934,    -1,   936,   140,   141,
     939,   143,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    -1,    -1,    -1,   719,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   727,   728,    -1,    -1,    -1,    -1,
      -1,   734,    -1,    -1,    -1,    -1,    -1,   740,   741,    -1,
      -1,    -1,    -1,   746,   747,    -1,    -1,    -1,    -1,     0,
      -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,    -1,    -1,
     763,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    -1,   775,   776,    -1,    -1,    27,    28,    -1,    -1,
     783,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,   798,   799,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    88,    -1,    90,
      91,    -1,    -1,    -1,   847,    -1,    97,    -1,    -1,    -1,
      -1,   102,    -1,    -1,   857,   858,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,   115,   116,   869,   118,   119,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,    -1,    -1,    -1,   136,   137,   138,    -1,    -1,
     141,   142,   143,   144,   145,    -1,    -1,    -1,    -1,    -1,
       0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,   928,    -1,    27,    28,    -1,
      -1,   934,    -1,   936,    -1,    -1,   939,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,   115,   116,    -1,    -1,   119,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,     0,    -1,    -1,   136,   137,   138,    -1,
     140,   141,   142,   143,   144,   145,    13,    14,    15,    -1,
      17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,
      -1,   118,   119,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,     0,    -1,   135,   136,
     137,   138,    -1,    -1,   141,    -1,   143,   144,   145,    13,
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,   115,   116,    -1,   118,   119,    -1,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,    -1,     0,
      -1,   135,   136,   137,   138,    -1,    -1,   141,    -1,   143,
     144,   145,    13,    14,    15,    -1,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    88,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,   115,   116,    -1,   118,   119,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,     0,    -1,    -1,   136,   137,   138,    -1,    -1,
     141,    -1,   143,   144,   145,    13,    14,    15,    -1,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,
     118,   119,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    -1,   136,   137,
     138,    -1,    -1,   141,    -1,   143,   144,   145,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    -1,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,     1,   145,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    -1,    17,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,     1,   145,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    -1,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,     1,   145,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,   112,    -1,
      -1,   115,   116,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   131,   132,   133,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   143,
      -1,   145,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   143,     1,   145,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    14,
      15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,
     115,   116,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   131,   132,   133,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,   143,    -1,
     145,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,   112,    -1,    -1,   115,   116,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   131,   132,   133,    -1,    19,    -1,    21,    22,
      23,    24,   141,    -1,   143,    -1,   145,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,   112,
      -1,    -1,   115,   116,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   131,   132,
     133,    -1,    19,    -1,    21,    22,    23,    24,   141,    -1,
     143,    -1,   145,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,   132,   133,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,     1,   145,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,   112,    -1,
      -1,   115,   116,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   131,   132,   133,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   143,
      -1,   145,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,   131,   132,   133,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,   143,    -1,   145,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
     112,    -1,    -1,   115,   116,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,   131,
     132,   133,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   143,    -1,   145,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    -1,    -1,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,
     116,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,   131,   132,   133,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   143,    -1,   145,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   145,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    -1,    -1,    -1,
      -1,    -1,   139,   140,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   131,   132,   133,    19,   135,    21,    22,    23,
      24,   140,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    99,   100,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,
      19,   135,    21,    22,    23,    24,   140,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   131,   132,   133,    19,   135,    21,    22,    23,
      24,   140,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    90,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    99,   100,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,    -1,   140,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    90,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    99,   100,   101,    -1,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,    -1,    -1,
      -1,    -1,    -1,    -1,   140,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    99,   100,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,    22,
      23,    24,   140,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    99,   100,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,   132,
     133,    19,    -1,    21,    22,    23,    24,   140,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    99,   100,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    -1,    -1,    -1,    -1,    -1,   139,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    -1,    -1,    -1,    -1,    -1,   139,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    -1,    -1,
      -1,    -1,    -1,   139,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    -1,    -1,    -1,    -1,    -1,
     139,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   131,
      21,    22,    23,    24,    -1,    -1,   138,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     131,    21,    22,    23,    24,    -1,    -1,   138,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,
     115,   116,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,
     115,   116,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   131,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   131,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   131,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   131,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     131,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   131,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,
      -1,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,    -1,    -1,    -1,    -1,   119,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    51,
      52,    -1,   131,    55,    -1,    -1,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,    71,
      72,    73,    74,    75,    76,    77,    98,    99,    80,    81,
      -1,    -1,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
     132,   133,    51,    52,    -1,    -1,    55,   139,   140,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    98,
      99,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    51,    52,    -1,    -1,    55,
     139,   140,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    51,    52,
      -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    51,    52,    -1,    -1,    55,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    51,    52,    -1,
      -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,   132,   133,
      51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,   132,   133,    51,    52,    -1,    -1,    55,   139,   140,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,   132,   133,    51,    52,    -1,    -1,
      55,   139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,    -1,   132,   133,    51,
      52,    -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
     132,   133,    51,    52,    -1,    -1,    55,   139,   140,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    51,    52,    -1,    -1,    55,
     139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    51,    52,
      -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    51,    52,    -1,    -1,    55,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    -1,    -1,    -1,
      -1,    -1,   139,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    -1,   119,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,   145,    86,    87,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   138,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   138,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    -1,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   138,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   138,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    -1,
      -1,    -1,    91,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   147,   148,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    89,    92,
      93,    95,    96,    98,   101,   103,   104,   105,   106,   107,
     108,   111,   131,   132,   133,   149,   150,   151,   156,   158,
     160,   162,   163,   166,   167,   169,   170,   171,   173,   174,
     183,   197,   218,   239,   240,   250,   251,   255,   256,   257,
     263,   264,   265,   267,   268,   269,   270,   271,   272,   305,
     319,   151,    21,    22,    30,    31,    32,    39,    51,    55,
      86,    89,    92,   131,   175,   176,   197,   218,   269,   272,
     305,   176,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    45,    46,    47,    48,
      49,    50,    51,    52,    55,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    80,    81,    84,    85,    86,    87,
      98,    99,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   132,   133,   139,   140,   177,   181,   182,   271,
     300,   198,    89,   160,   161,   174,   218,   269,   270,   272,
     161,   204,   206,    89,   167,   174,   218,   223,   269,   272,
      33,    34,    35,    36,    48,    49,    50,    51,    55,   103,
     177,   178,   179,   265,   112,   115,   116,   143,   145,   161,
     259,   260,   261,   311,   316,   317,   318,    51,    98,    99,
     100,   132,   166,   183,   189,   192,   195,   251,   303,   304,
     189,   189,   140,   186,   187,   190,   191,   319,   186,   190,
     140,   312,   317,   178,   152,   135,   183,   218,   183,    55,
       1,    92,   154,   155,   156,   168,   169,   319,   199,   201,
     184,   195,   303,   319,   183,   302,   303,   319,    89,   138,
     173,   218,   269,   272,   202,    53,    54,    56,    63,   107,
     177,   266,    62,    64,    65,   113,   114,   252,   253,    63,
     252,    63,   252,    63,   252,    61,   252,    58,    59,   162,
     183,   183,   311,   318,    40,    41,    42,    43,    44,    37,
      38,    28,   237,   118,   138,    92,    98,   170,   118,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    86,    87,   119,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    88,   102,   137,   144,   309,
      88,   309,   310,    26,   135,   241,    90,    90,   186,   190,
     241,   160,    51,    55,   175,    58,    59,   122,   273,    88,
     137,   309,   213,   301,   214,    88,   144,   308,   153,   154,
      55,    16,   219,   316,   118,    88,   137,   309,    90,    90,
     219,   161,   161,    55,    88,   137,   309,    25,   107,   138,
     262,   311,   112,   261,    20,   244,   316,    57,   183,   183,
     183,    91,   138,   193,   194,   319,    57,   138,   193,   194,
     188,   189,   195,   303,   319,   189,   160,   312,   313,   160,
     157,   135,   154,    88,   309,    90,   156,   168,   141,   311,
     318,   313,   156,   313,   142,   194,   315,   317,   194,   315,
     136,   315,    55,   170,   171,   172,   138,    88,   137,   309,
      51,    53,    54,    55,    56,    71,    92,    98,    99,   100,
     125,   128,   140,   235,   276,   277,   280,   281,   282,   283,
     285,   286,   287,   288,   289,   290,   291,   294,   295,   296,
     297,   298,    63,   252,   254,   258,   259,    62,   253,    63,
      63,    63,    61,    71,    71,   151,   161,   161,   161,   161,
     156,   160,   160,   238,    98,   162,   183,   195,   196,   168,
     138,   173,   138,   158,   159,   162,   174,   183,   185,   196,
     218,   272,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,    51,    52,
      55,   181,   186,   306,   307,   188,    51,    52,    55,   181,
     186,   306,    51,    55,   306,   243,   242,   159,   183,   185,
     159,   185,    97,   164,   211,   274,   210,    51,    55,   175,
     306,   188,   306,   153,   160,   215,   216,    15,    13,   246,
     319,   154,    16,    51,    55,   188,    51,    55,   154,    27,
     220,   316,   220,    51,    55,   188,    51,    55,   208,   180,
     154,   244,   183,   195,    15,   183,   183,   258,    98,   183,
     192,   303,   183,   304,   313,   138,   194,   138,   313,   141,
     178,   149,   136,   185,   313,   156,   200,   303,   170,   172,
      51,    55,   188,    51,    55,    57,   118,    51,    92,    98,
     224,   225,   226,   287,   285,   203,   183,   138,   299,   319,
      51,   138,   299,   138,   284,   183,   138,   284,    51,   138,
     284,    51,    63,   154,   259,   183,   183,    79,   123,   230,
     231,   319,   183,   194,   313,   172,   138,    44,   118,    44,
      88,   137,   309,   312,    90,    90,   186,   190,   312,   314,
      90,    90,   187,   190,   187,   190,   230,   230,   165,   316,
     161,   153,   314,    15,   313,   140,   275,   285,   177,   183,
     196,   247,   319,    18,   222,   319,    17,   221,   222,    90,
      90,   314,    90,    90,   222,   205,   207,   314,   161,   178,
     136,    15,   194,   219,   183,   193,   303,   136,   313,   315,
     314,   226,   138,   287,   138,   313,   232,   312,    29,   110,
     236,    51,   277,   282,   298,   283,   288,   294,   296,   289,
     291,   296,    51,   289,   136,   227,   229,   232,   276,   278,
     279,   282,   289,   290,   292,   293,   296,   298,   153,    98,
     183,   172,   156,   183,    51,    55,   188,    51,    55,    57,
     120,   159,   185,   162,   185,   164,   142,    90,   159,   185,
     159,   185,   164,   241,   237,   153,   154,   230,   212,   316,
      15,   285,   153,   316,   217,    91,   248,   319,   154,    14,
     249,   319,   161,    15,    90,    15,   154,   154,   220,   183,
     154,   138,   313,   225,   138,    98,   224,   141,   143,   153,
     154,   299,   138,   284,   138,   284,   138,   284,   138,   284,
     284,   232,   123,    89,   218,   138,   299,   299,   138,   228,
     218,   138,   228,   138,   228,    15,   183,   314,   183,   183,
     159,   185,    15,   136,   154,   153,   313,    15,   275,    89,
     174,   218,   269,   272,   219,   154,   219,    15,    15,   209,
     222,   244,   245,   138,   225,   138,   287,    51,   233,   234,
     286,    15,   136,   289,   296,   289,   289,   123,    55,    88,
     278,   282,   227,   293,   296,   289,   292,   296,   289,   136,
      15,   153,    55,    88,   137,   309,   154,   154,   154,   225,
     138,   138,   312,   284,   138,   284,   284,   284,    51,    55,
     299,   138,   228,   138,   228,   138,   228,   138,   228,   228,
      15,    51,    55,   188,    51,    55,   246,   221,    15,   225,
     234,   289,   289,   296,   289,   289,   314,   284,   228,   138,
     228,   228,   228,   289,   228
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (p, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, p)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, p); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, p)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    parser_state *p;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (p);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, p)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    parser_state *p;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, p);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule, parser_state *p)
#else
static void
yy_reduce_print (yyvsp, yyrule, p)
    YYSTYPE *yyvsp;
    int yyrule;
    parser_state *p;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       , p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule, p); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, parser_state *p)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, p)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    parser_state *p;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (p);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (parser_state *p);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */





/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (parser_state *p)
#else
int
yyparse (p)
    parser_state *p;
#endif
#endif
{
/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 1251 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 1256 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->tree = new_scope(p, (yyvsp[(2) - (2)].nd));
                      NODE_LINENO(p->tree, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 1263 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 1269 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 1273 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 1278 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), newline_node((yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 1282 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 1289 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = local_switch(p);
                    ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 1293 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[(2) - (5)].nd));
                      (yyval.nd) = 0;
                    ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 1304 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if ((yyvsp[(2) - (4)].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[(1) - (4)].nd), (yyvsp[(2) - (4)].nd), (yyvsp[(3) - (4)].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                      }
                      else if ((yyvsp[(3) - (4)].nd)) {
                        yywarn(p, "else without rescue is useless");
                        (yyval.nd) = push((yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd));
                      }
                      else {
                        (yyval.nd) = (yyvsp[(1) - (4)].nd);
                      }
                      if ((yyvsp[(4) - (4)].nd)) {
                        if ((yyval.nd)) {
                          (yyval.nd) = new_ensure(p, (yyval.nd), (yyvsp[(4) - (4)].nd));
                        }
                        else {
                          (yyval.nd) = push((yyvsp[(4) - (4)].nd), new_nil(p));
                        }
                      }
                    ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 1328 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 1334 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 1338 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 1343 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), newline_node((yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 1347 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 1352 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {p->lstate = EXPR_FNAME;;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 1353 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_alias(p, (yyvsp[(2) - (4)].id), (yyvsp[(4) - (4)].id));
                    ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 1357 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 1361 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd), 0);
                    ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 1365 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd), 0);
                    ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 1369 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd));
                    ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 1373 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd));
                    ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 1377 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 1381 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[(3) - (4)].nd));
                    ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 1387 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 1391 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), new_array(p, (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 1395 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 1399 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), new_array(p, (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 1406 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1410 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 1414 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (6)].nd), intern("[]",2), (yyvsp[(3) - (6)].nd), '.'), (yyvsp[(5) - (6)].id), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1418 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, (yyvsp[(2) - (5)].num)), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 1422 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, (yyvsp[(2) - (5)].num)), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 1426 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1431 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, tCOLON2), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 1435 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (3)].nd));
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1443 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 1452 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_and(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1456 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_or(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1460 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(3) - (3)].nd)), "!");
                    ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1464 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "!");
                    ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1471 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (!(yyvsp[(1) - (1)].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      }
                    ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 1485 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), (yyvsp[(2) - (4)].num));
                    ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 1491 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_nest(p);
                    ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 1497 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_block(p, (yyvsp[(3) - (5)].nd), (yyvsp[(4) - (5)].nd));
                      local_unnest(p);
                    ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1504 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1508 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      args_with_block(p, (yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (3)].id), (yyvsp[(2) - (3)].nd));
                    ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1513 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), (yyvsp[(2) - (4)].num));
                    ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1517 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      args_with_block(p, (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd), (yyvsp[(2) - (5)].num));
                   ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1522 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), tCOLON2);
                    ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1526 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      args_with_block(p, (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd), tCOLON2);
                    ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1531 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_super(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1535 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_yield(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1539 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1543 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1547 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1553 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1557 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 1564 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1570 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1574 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1(push((yyvsp[(1) - (2)].nd),(yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1578 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list2((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1582 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].nd), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1586 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list2((yyvsp[(1) - (2)].nd), new_nil(p));
                    ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1590 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (4)].nd), new_nil(p), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1594 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list2(0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1598 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, (yyvsp[(2) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1602 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list2(0, new_nil(p));
                    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1606 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1613 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), NULL);
                    ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1619 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1623 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].nd));
                    ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1629 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1633 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1639 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1643 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd), '.');
                    ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1647 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, (yyvsp[(2) - (3)].num));
                    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1651 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, tCOLON2);
                    ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1655 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, (yyvsp[(2) - (3)].num));
                    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1659 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1665 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1671 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = 0;
                    ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1678 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1682 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd), '.');
                    ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1686 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, (yyvsp[(2) - (3)].num));
                    ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1690 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, tCOLON2);
                    ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1694 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, (yyvsp[(2) - (3)].num));
                    ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1698 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1704 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1710 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = 0;
                    ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1717 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "class/module name must be CONSTANT");
                    ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1724 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[(2) - (2)].id)));
                    ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1728 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[(1) - (1)].id)));
                    ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1732 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(1) - (3)].nd));
                      (yyval.nd) = cons((yyvsp[(1) - (3)].nd), nsym((yyvsp[(3) - (3)].id)));
                    ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1742 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1747 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1758 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_undef(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1761 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {p->lstate = EXPR_FNAME;;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1762 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), nsym((yyvsp[(4) - (4)].id)));
                    ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1767 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('|');   ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1768 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('^');   ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1769 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('&');   ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1770 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("<=>",3); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1771 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("==",2);  ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1772 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("===",3); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1773 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("=~",2);  ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1774 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("!~",2);  ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1775 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('>');   ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1776 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern(">=",2);  ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1777 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('<');   ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1778 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("<=",2);  ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1779 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("!=",2);  ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 1780 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("<<",2);  ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 1781 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern(">>",2);  ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1782 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('+');   ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1783 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('-');   ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1784 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('*');   ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1785 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('*');   ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1786 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('/');   ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1787 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('%');   ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1788 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("**",2);  ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1789 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("**",2);  ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1790 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('!');   ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1791 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('~');   ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1792 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("+@",2);  ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1793 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("-@",2);  ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 1794 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("[]",2);  ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1795 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern("[]=",3); ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1796 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    { (yyval.id) = intern_c('`');   ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 1814 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 1818 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 1822 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (6)].nd), intern("[]",2), (yyvsp[(3) - (6)].nd), '.'), (yyvsp[(5) - (6)].id), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 1826 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, (yyvsp[(2) - (5)].num)), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 1830 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, (yyvsp[(2) - (5)].num)), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 1834 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0, tCOLON2), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 1838 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 1843 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 1848 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (3)].nd));
                      (yyval.nd) = new_begin(p, 0);
                    ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 1853 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_dot2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 1857 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_dot3(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 1861 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "+", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 1865 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "-", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 1869 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "*", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1873 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "/", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1877 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "%", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 1881 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "**", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 1885 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[(2) - (4)].nd), "**", (yyvsp[(4) - (4)].nd)), "-@");
                    ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 1889 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[(2) - (4)].nd), "**", (yyvsp[(4) - (4)].nd)), "-@");
                    ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 1893 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[(2) - (2)].nd), "+@");
                    ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 1897 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[(2) - (2)].nd), "-@");
                    ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 1901 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "|", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 1905 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "^", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 1909 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "&", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 1913 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<=>", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 1917 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 1921 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">=", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 1925 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 1929 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<=", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 1933 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "==", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 1937 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "===", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 1941 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "!=", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 1945 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "=~", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 1949 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "!~", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 1953 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "!");
                    ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 1957 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "~");
                    ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 1961 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<<", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 1965 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">>", (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 227:

/* Line 1455 of yacc.c  */
#line 1969 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_and(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 1973 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_or(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 229:

/* Line 1455 of yacc.c  */
#line 1977 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(1) - (6)].nd)), (yyvsp[(3) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 230:

/* Line 1455 of yacc.c  */
#line 1981 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(1) - (6)].nd)), (yyvsp[(3) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 231:

/* Line 1455 of yacc.c  */
#line 1985 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    ;}
    break;

  case 233:

/* Line 1455 of yacc.c  */
#line 1992 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 234:

/* Line 1455 of yacc.c  */
#line 1997 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_kw_hash(p, (yyvsp[(3) - (4)].nd)));
                    ;}
    break;

  case 235:

/* Line 1455 of yacc.c  */
#line 2001 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[(1) - (2)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 236:

/* Line 1455 of yacc.c  */
#line 2008 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    ;}
    break;

  case 237:

/* Line 1455 of yacc.c  */
#line 2012 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(1) - (3)].nd));
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 238:

/* Line 1455 of yacc.c  */
#line 2020 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 243:

/* Line 1455 of yacc.c  */
#line 2032 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (2)].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 244:

/* Line 1455 of yacc.c  */
#line 2037 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(push((yyvsp[(1) - (4)].nd), new_kw_hash(p, (yyvsp[(3) - (4)].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                    ;}
    break;

  case 245:

/* Line 1455 of yacc.c  */
#line 2042 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[(1) - (2)].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 246:

/* Line 1455 of yacc.c  */
#line 2049 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = cons(list1((yyvsp[(1) - (1)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 247:

/* Line 1455 of yacc.c  */
#line 2055 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 248:

/* Line 1455 of yacc.c  */
#line 2060 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[(1) - (2)].nd))), (yyvsp[(2) - (2)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    ;}
    break;

  case 249:

/* Line 1455 of yacc.c  */
#line 2065 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(push((yyvsp[(1) - (4)].nd), new_kw_hash(p, (yyvsp[(3) - (4)].nd))), (yyvsp[(4) - (4)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                    ;}
    break;

  case 250:

/* Line 1455 of yacc.c  */
#line 2070 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(0, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 251:

/* Line 1455 of yacc.c  */
#line 2076 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    ;}
    break;

  case 252:

/* Line 1455 of yacc.c  */
#line 2081 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(1) - (2)].stack);
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 253:

/* Line 1455 of yacc.c  */
#line 2088 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 254:

/* Line 1455 of yacc.c  */
#line 2094 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 255:

/* Line 1455 of yacc.c  */
#line 2098 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 258:

/* Line 1455 of yacc.c  */
#line 2108 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = cons((yyvsp[(1) - (1)].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 259:

/* Line 1455 of yacc.c  */
#line 2114 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[(2) - (2)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 260:

/* Line 1455 of yacc.c  */
#line 2120 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 261:

/* Line 1455 of yacc.c  */
#line 2125 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(4) - (4)].nd));
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_splat(p, (yyvsp[(4) - (4)].nd)));
                    ;}
    break;

  case 262:

/* Line 1455 of yacc.c  */
#line 2132 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 263:

/* Line 1455 of yacc.c  */
#line 2137 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(4) - (4)].nd));
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_splat(p, (yyvsp[(4) - (4)].nd)));
                    ;}
    break;

  case 264:

/* Line 1455 of yacc.c  */
#line 2142 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 272:

/* Line 1455 of yacc.c  */
#line 2156 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (1)].id), 0);
                    ;}
    break;

  case 273:

/* Line 1455 of yacc.c  */
#line 2160 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    ;}
    break;

  case 274:

/* Line 1455 of yacc.c  */
#line 2166 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(2) - (4)].stack);
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    ;}
    break;

  case 275:

/* Line 1455 of yacc.c  */
#line 2171 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    ;}
    break;

  case 276:

/* Line 1455 of yacc.c  */
#line 2175 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {p->lstate = EXPR_ENDARG;;}
    break;

  case 277:

/* Line 1455 of yacc.c  */
#line 2176 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(2) - (5)].stack);
                      (yyval.nd) = (yyvsp[(3) - (5)].nd);
                    ;}
    break;

  case 278:

/* Line 1455 of yacc.c  */
#line 2180 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {p->lstate = EXPR_ENDARG;;}
    break;

  case 279:

/* Line 1455 of yacc.c  */
#line 2181 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_nil(p);
                    ;}
    break;

  case 280:

/* Line 1455 of yacc.c  */
#line 2185 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 281:

/* Line 1455 of yacc.c  */
#line 2189 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    ;}
    break;

  case 282:

/* Line 1455 of yacc.c  */
#line 2193 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 283:

/* Line 1455 of yacc.c  */
#line 2197 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_array(p, (yyvsp[(2) - (3)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (3)].nd));
                    ;}
    break;

  case 284:

/* Line 1455 of yacc.c  */
#line 2202 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_hash(p, (yyvsp[(2) - (3)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (3)].nd));
                    ;}
    break;

  case 285:

/* Line 1455 of yacc.c  */
#line 2207 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_return(p, 0);
                    ;}
    break;

  case 286:

/* Line 1455 of yacc.c  */
#line 2211 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_yield(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 287:

/* Line 1455 of yacc.c  */
#line 2215 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(3) - (4)].nd)), "!");
                    ;}
    break;

  case 288:

/* Line 1455 of yacc.c  */
#line 2219 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    ;}
    break;

  case 289:

/* Line 1455 of yacc.c  */
#line 2223 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), cons(0, (yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 291:

/* Line 1455 of yacc.c  */
#line 2228 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      call_with_block(p, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 292:

/* Line 1455 of yacc.c  */
#line 2233 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    ;}
    break;

  case 293:

/* Line 1455 of yacc.c  */
#line 2239 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    ;}
    break;

  case 294:

/* Line 1455 of yacc.c  */
#line 2244 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lpar_beg = (yyvsp[(2) - (5)].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[(3) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[(4) - (5)].stack);
                      CMDARG_LEXPOP();
                    ;}
    break;

  case 295:

/* Line 1455 of yacc.c  */
#line 2255 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(2) - (6)].nd)), (yyvsp[(4) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                    ;}
    break;

  case 296:

/* Line 1455 of yacc.c  */
#line 2263 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[(2) - (6)].nd)), (yyvsp[(4) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                    ;}
    break;

  case 297:

/* Line 1455 of yacc.c  */
#line 2267 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_PUSH(1);;}
    break;

  case 298:

/* Line 1455 of yacc.c  */
#line 2267 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_POP();;}
    break;

  case 299:

/* Line 1455 of yacc.c  */
#line 2270 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[(3) - (7)].nd)), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                    ;}
    break;

  case 300:

/* Line 1455 of yacc.c  */
#line 2274 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_PUSH(1);;}
    break;

  case 301:

/* Line 1455 of yacc.c  */
#line 2274 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_POP();;}
    break;

  case 302:

/* Line 1455 of yacc.c  */
#line 2277 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[(3) - (7)].nd)), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                    ;}
    break;

  case 303:

/* Line 1455 of yacc.c  */
#line 2284 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_case(p, (yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd));
                    ;}
    break;

  case 304:

/* Line 1455 of yacc.c  */
#line 2288 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[(3) - (4)].nd));
                    ;}
    break;

  case 305:

/* Line 1455 of yacc.c  */
#line 2292 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_PUSH(1);;}
    break;

  case 306:

/* Line 1455 of yacc.c  */
#line 2294 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {COND_POP();;}
    break;

  case 307:

/* Line 1455 of yacc.c  */
#line 2297 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_for(p, (yyvsp[(2) - (9)].nd), (yyvsp[(5) - (9)].nd), (yyvsp[(8) - (9)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (9)].num));
                    ;}
    break;

  case 308:

/* Line 1455 of yacc.c  */
#line 2303 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                    ;}
    break;

  case 309:

/* Line 1455 of yacc.c  */
#line 2310 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_class(p, (yyvsp[(2) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                      local_resume(p, (yyvsp[(4) - (6)].nd));
                    ;}
    break;

  case 310:

/* Line 1455 of yacc.c  */
#line 2317 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    ;}
    break;

  case 311:

/* Line 1455 of yacc.c  */
#line 2322 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      p->in_single = 0;
                    ;}
    break;

  case 312:

/* Line 1455 of yacc.c  */
#line 2328 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_sclass(p, (yyvsp[(3) - (8)].nd), (yyvsp[(7) - (8)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (8)].num));
                      local_resume(p, (yyvsp[(6) - (8)].nd)->car);
                      p->in_def = (yyvsp[(4) - (8)].num);
                      p->in_single = intn((yyvsp[(6) - (8)].nd)->cdr);
                    ;}
    break;

  case 313:

/* Line 1455 of yacc.c  */
#line 2337 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                    ;}
    break;

  case 314:

/* Line 1455 of yacc.c  */
#line 2344 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_module(p, (yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (5)].num));
                      local_resume(p, (yyvsp[(3) - (5)].nd));
                    ;}
    break;

  case 315:

/* Line 1455 of yacc.c  */
#line 2350 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    ;}
    break;

  case 316:

/* Line 1455 of yacc.c  */
#line 2354 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->in_def++;
                      (yyval.nd) = local_switch(p);
                    ;}
    break;

  case 317:

/* Line 1455 of yacc.c  */
#line 2361 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_def(p, (yyvsp[(2) - (7)].id), (yyvsp[(5) - (7)].nd), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                      local_resume(p, (yyvsp[(4) - (7)].nd));
                      p->in_def--;
                      p->cmdarg_stack = (yyvsp[(3) - (7)].stack);
                    ;}
    break;

  case 318:

/* Line 1455 of yacc.c  */
#line 2369 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_FNAME;
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    ;}
    break;

  case 319:

/* Line 1455 of yacc.c  */
#line 2375 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->in_single++;
                      p->lstate = EXPR_ENDFN; /* force for args */
                      (yyval.nd) = local_switch(p);
                    ;}
    break;

  case 320:

/* Line 1455 of yacc.c  */
#line 2383 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_sdef(p, (yyvsp[(2) - (9)].nd), (yyvsp[(5) - (9)].id), (yyvsp[(7) - (9)].nd), (yyvsp[(8) - (9)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (9)].num));
                      local_resume(p, (yyvsp[(6) - (9)].nd));
                      p->in_single--;
                      p->cmdarg_stack = (yyvsp[(4) - (9)].stack);
                    ;}
    break;

  case 321:

/* Line 1455 of yacc.c  */
#line 2391 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_break(p, 0);
                    ;}
    break;

  case 322:

/* Line 1455 of yacc.c  */
#line 2395 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_next(p, 0);
                    ;}
    break;

  case 323:

/* Line 1455 of yacc.c  */
#line 2399 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_redo(p);
                    ;}
    break;

  case 324:

/* Line 1455 of yacc.c  */
#line 2403 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_retry(p);
                    ;}
    break;

  case 325:

/* Line 1455 of yacc.c  */
#line 2409 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    ;}
    break;

  case 332:

/* Line 1455 of yacc.c  */
#line 2428 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(2) - (5)].nd)), (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 334:

/* Line 1455 of yacc.c  */
#line 2435 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 335:

/* Line 1455 of yacc.c  */
#line 2441 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1(list1((yyvsp[(1) - (1)].nd)));
                    ;}
    break;

  case 337:

/* Line 1455 of yacc.c  */
#line 2448 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_arg(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 338:

/* Line 1455 of yacc.c  */
#line 2452 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), 0);
                    ;}
    break;

  case 339:

/* Line 1455 of yacc.c  */
#line 2458 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 340:

/* Line 1455 of yacc.c  */
#line 2462 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 341:

/* Line 1455 of yacc.c  */
#line 2468 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (1)].nd),0,0);
                    ;}
    break;

  case 342:

/* Line 1455 of yacc.c  */
#line 2472 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (4)].nd), new_arg(p, (yyvsp[(4) - (4)].id)), 0);
                    ;}
    break;

  case 343:

/* Line 1455 of yacc.c  */
#line 2476 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (6)].nd), new_arg(p, (yyvsp[(4) - (6)].id)), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 344:

/* Line 1455 of yacc.c  */
#line 2480 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (3)].nd), (node*)-1, 0);
                    ;}
    break;

  case 345:

/* Line 1455 of yacc.c  */
#line 2484 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (5)].nd), (node*)-1, (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 346:

/* Line 1455 of yacc.c  */
#line 2488 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[(2) - (2)].id)), 0);
                    ;}
    break;

  case 347:

/* Line 1455 of yacc.c  */
#line 2492 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[(2) - (4)].id)), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 348:

/* Line 1455 of yacc.c  */
#line 2496 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    ;}
    break;

  case 349:

/* Line 1455 of yacc.c  */
#line 2500 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 350:

/* Line 1455 of yacc.c  */
#line 2506 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    ;}
    break;

  case 351:

/* Line 1455 of yacc.c  */
#line 2510 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[(1) - (2)].nd), 0, (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 352:

/* Line 1455 of yacc.c  */
#line 2514 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 353:

/* Line 1455 of yacc.c  */
#line 2518 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 354:

/* Line 1455 of yacc.c  */
#line 2524 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 355:

/* Line 1455 of yacc.c  */
#line 2528 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    ;}
    break;

  case 356:

/* Line 1455 of yacc.c  */
#line 2534 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].id), 0, (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 357:

/* Line 1455 of yacc.c  */
#line 2538 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (8)].nd), (yyvsp[(3) - (8)].nd), (yyvsp[(5) - (8)].id), (yyvsp[(7) - (8)].nd), (yyvsp[(8) - (8)].nd));
                    ;}
    break;

  case 358:

/* Line 1455 of yacc.c  */
#line 2542 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), 0, 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 359:

/* Line 1455 of yacc.c  */
#line 2546 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), 0, (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 360:

/* Line 1455 of yacc.c  */
#line 2550 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 361:

/* Line 1455 of yacc.c  */
#line 2554 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 0, 0, 0);
                    ;}
    break;

  case 362:

/* Line 1455 of yacc.c  */
#line 2558 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), 0, (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 363:

/* Line 1455 of yacc.c  */
#line 2562 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 0, 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 364:

/* Line 1455 of yacc.c  */
#line 2566 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 365:

/* Line 1455 of yacc.c  */
#line 2570 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 366:

/* Line 1455 of yacc.c  */
#line 2574 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (2)].nd), 0, 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 367:

/* Line 1455 of yacc.c  */
#line 2578 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 368:

/* Line 1455 of yacc.c  */
#line 2582 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (2)].id), 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 369:

/* Line 1455 of yacc.c  */
#line 2586 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (4)].id), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 370:

/* Line 1455 of yacc.c  */
#line 2590 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 372:

/* Line 1455 of yacc.c  */
#line 2597 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    ;}
    break;

  case 373:

/* Line 1455 of yacc.c  */
#line 2604 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 374:

/* Line 1455 of yacc.c  */
#line 2608 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 375:

/* Line 1455 of yacc.c  */
#line 2612 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (4)].nd);
                    ;}
    break;

  case 376:

/* Line 1455 of yacc.c  */
#line 2619 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 377:

/* Line 1455 of yacc.c  */
#line 2623 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 380:

/* Line 1455 of yacc.c  */
#line 2633 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (1)].id));
                      new_bv(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 382:

/* Line 1455 of yacc.c  */
#line 2641 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (4)].nd);
                    ;}
    break;

  case 383:

/* Line 1455 of yacc.c  */
#line 2645 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    ;}
    break;

  case 384:

/* Line 1455 of yacc.c  */
#line 2651 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 385:

/* Line 1455 of yacc.c  */
#line 2655 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    ;}
    break;

  case 386:

/* Line 1455 of yacc.c  */
#line 2661 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_nest(p);
                    ;}
    break;

  case 387:

/* Line 1455 of yacc.c  */
#line 2667 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      local_unnest(p);
                    ;}
    break;

  case 388:

/* Line 1455 of yacc.c  */
#line 2674 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if ((yyvsp[(1) - (2)].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      }
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 389:

/* Line 1455 of yacc.c  */
#line 2684 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), (yyvsp[(2) - (4)].num));
                    ;}
    break;

  case 390:

/* Line 1455 of yacc.c  */
#line 2688 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd), (yyvsp[(2) - (5)].num));
                      call_with_block(p, (yyval.nd), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 391:

/* Line 1455 of yacc.c  */
#line 2693 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd), (yyvsp[(2) - (5)].num));
                      call_with_block(p, (yyval.nd), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 392:

/* Line 1455 of yacc.c  */
#line 2700 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 393:

/* Line 1455 of yacc.c  */
#line 2704 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), (yyvsp[(2) - (4)].num));
                    ;}
    break;

  case 394:

/* Line 1455 of yacc.c  */
#line 2708 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd), tCOLON2);
                    ;}
    break;

  case 395:

/* Line 1455 of yacc.c  */
#line 2712 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0, tCOLON2);
                    ;}
    break;

  case 396:

/* Line 1455 of yacc.c  */
#line 2716 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), intern("call",4), (yyvsp[(3) - (3)].nd), (yyvsp[(2) - (3)].num));
                    ;}
    break;

  case 397:

/* Line 1455 of yacc.c  */
#line 2720 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), intern("call",4), (yyvsp[(3) - (3)].nd), tCOLON2);
                    ;}
    break;

  case 398:

/* Line 1455 of yacc.c  */
#line 2724 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_super(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 399:

/* Line 1455 of yacc.c  */
#line 2728 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_zsuper(p);
                    ;}
    break;

  case 400:

/* Line 1455 of yacc.c  */
#line 2732 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd), '.');
                    ;}
    break;

  case 401:

/* Line 1455 of yacc.c  */
#line 2738 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    ;}
    break;

  case 402:

/* Line 1455 of yacc.c  */
#line 2744 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(2) - (5)].num));
                      local_unnest(p);
                    ;}
    break;

  case 403:

/* Line 1455 of yacc.c  */
#line 2750 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    ;}
    break;

  case 404:

/* Line 1455 of yacc.c  */
#line 2756 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(2) - (5)].num));
                      local_unnest(p);
                    ;}
    break;

  case 405:

/* Line 1455 of yacc.c  */
#line 2766 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons(cons((yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd)), (yyvsp[(5) - (5)].nd));
                    ;}
    break;

  case 406:

/* Line 1455 of yacc.c  */
#line 2772 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if ((yyvsp[(1) - (1)].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[(1) - (1)].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    ;}
    break;

  case 408:

/* Line 1455 of yacc.c  */
#line 2786 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1(list3((yyvsp[(2) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].nd)));
                      if ((yyvsp[(6) - (6)].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 410:

/* Line 1455 of yacc.c  */
#line 2794 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                        (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 413:

/* Line 1455 of yacc.c  */
#line 2802 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 415:

/* Line 1455 of yacc.c  */
#line 2809 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 423:

/* Line 1455 of yacc.c  */
#line 2824 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 424:

/* Line 1455 of yacc.c  */
#line 2828 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 426:

/* Line 1455 of yacc.c  */
#line 2835 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = append((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 427:

/* Line 1455 of yacc.c  */
#line 2841 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 428:

/* Line 1455 of yacc.c  */
#line 2845 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    ;}
    break;

  case 429:

/* Line 1455 of yacc.c  */
#line 2851 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lex_strterm = (yyvsp[(2) - (4)].nd);
                      (yyval.nd) = list2((yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd));
                    ;}
    break;

  case 430:

/* Line 1455 of yacc.c  */
#line 2856 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    ;}
    break;

  case 431:

/* Line 1455 of yacc.c  */
#line 2860 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    ;}
    break;

  case 432:

/* Line 1455 of yacc.c  */
#line 2866 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                        (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 433:

/* Line 1455 of yacc.c  */
#line 2870 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 434:

/* Line 1455 of yacc.c  */
#line 2876 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                        (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 435:

/* Line 1455 of yacc.c  */
#line 2880 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_dregx(p, (yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 439:

/* Line 1455 of yacc.c  */
#line 2893 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    ;}
    break;

  case 440:

/* Line 1455 of yacc.c  */
#line 2899 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      heredoc_end(p);
                    ;}
    break;

  case 443:

/* Line 1455 of yacc.c  */
#line 2909 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[(1) - (1)].nd));
                      heredoc_treat_nextline(p);
                    ;}
    break;

  case 444:

/* Line 1455 of yacc.c  */
#line 2915 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    ;}
    break;

  case 445:

/* Line 1455 of yacc.c  */
#line 2921 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[(2) - (4)].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[(1) - (4)].nd)), (yyvsp[(3) - (4)].nd));
                    ;}
    break;

  case 446:

/* Line 1455 of yacc.c  */
#line 2929 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_words(p, list1((yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 447:

/* Line 1455 of yacc.c  */
#line 2933 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_words(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 448:

/* Line 1455 of yacc.c  */
#line 2940 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 449:

/* Line 1455 of yacc.c  */
#line 2945 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd))));
                    ;}
    break;

  case 450:

/* Line 1455 of yacc.c  */
#line 2952 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    ;}
    break;

  case 455:

/* Line 1455 of yacc.c  */
#line 2962 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = new_strsym(p, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 456:

/* Line 1455 of yacc.c  */
#line 2966 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = new_strsym(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 457:

/* Line 1455 of yacc.c  */
#line 2972 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[(2) - (2)].nd)));
                    ;}
    break;

  case 458:

/* Line 1455 of yacc.c  */
#line 2976 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    ;}
    break;

  case 461:

/* Line 1455 of yacc.c  */
#line 2984 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 462:

/* Line 1455 of yacc.c  */
#line 2988 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 463:

/* Line 1455 of yacc.c  */
#line 2994 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_lvar(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 464:

/* Line 1455 of yacc.c  */
#line 2998 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_ivar(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 465:

/* Line 1455 of yacc.c  */
#line 3002 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_gvar(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 466:

/* Line 1455 of yacc.c  */
#line 3006 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_cvar(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 467:

/* Line 1455 of yacc.c  */
#line 3010 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_const(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 468:

/* Line 1455 of yacc.c  */
#line 3016 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 469:

/* Line 1455 of yacc.c  */
#line 3022 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = var_reference(p, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 470:

/* Line 1455 of yacc.c  */
#line 3026 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_nil(p);
                    ;}
    break;

  case 471:

/* Line 1455 of yacc.c  */
#line 3030 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_self(p);
                    ;}
    break;

  case 472:

/* Line 1455 of yacc.c  */
#line 3034 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_true(p);
                    ;}
    break;

  case 473:

/* Line 1455 of yacc.c  */
#line 3038 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_false(p);
                    ;}
    break;

  case 474:

/* Line 1455 of yacc.c  */
#line 3042 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      const char *fn = p->filename;
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    ;}
    break;

  case 475:

/* Line 1455 of yacc.c  */
#line 3050 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      char buf[16];

                      snprintf(buf, sizeof(buf), "%d", p->lineno);
                      (yyval.nd) = new_int(p, buf, 10);
                    ;}
    break;

  case 476:

/* Line 1455 of yacc.c  */
#line 3057 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    ;}
    break;

  case 479:

/* Line 1455 of yacc.c  */
#line 3072 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;

  case 480:

/* Line 1455 of yacc.c  */
#line 3076 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    ;}
    break;

  case 481:

/* Line 1455 of yacc.c  */
#line 3081 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    ;}
    break;

  case 482:

/* Line 1455 of yacc.c  */
#line 3092 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    ;}
    break;

  case 483:

/* Line 1455 of yacc.c  */
#line 3098 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 485:

/* Line 1455 of yacc.c  */
#line 3107 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 486:

/* Line 1455 of yacc.c  */
#line 3112 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[(1) - (1)].id), 0);
                    ;}
    break;

  case 487:

/* Line 1455 of yacc.c  */
#line 3118 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 488:

/* Line 1455 of yacc.c  */
#line 3122 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[(1) - (1)].id), 0);
                    ;}
    break;

  case 489:

/* Line 1455 of yacc.c  */
#line 3128 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 490:

/* Line 1455 of yacc.c  */
#line 3132 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 491:

/* Line 1455 of yacc.c  */
#line 3138 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 492:

/* Line 1455 of yacc.c  */
#line 3142 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 495:

/* Line 1455 of yacc.c  */
#line 3152 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((node*)NODE_KW_REST_ARGS, nsym((yyvsp[(2) - (2)].id)));
                    ;}
    break;

  case 496:

/* Line 1455 of yacc.c  */
#line 3156 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = cons((node*)NODE_KW_REST_ARGS, 0);
                    ;}
    break;

  case 497:

/* Line 1455 of yacc.c  */
#line 3162 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    ;}
    break;

  case 498:

/* Line 1455 of yacc.c  */
#line 3166 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[(1) - (2)].nd), 0, (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 499:

/* Line 1455 of yacc.c  */
#line 3170 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].id));
                    ;}
    break;

  case 500:

/* Line 1455 of yacc.c  */
#line 3174 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 501:

/* Line 1455 of yacc.c  */
#line 3180 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    ;}
    break;

  case 502:

/* Line 1455 of yacc.c  */
#line 3184 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    ;}
    break;

  case 503:

/* Line 1455 of yacc.c  */
#line 3190 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].id), 0, (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 504:

/* Line 1455 of yacc.c  */
#line 3194 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (8)].nd), (yyvsp[(3) - (8)].nd), (yyvsp[(5) - (8)].id), (yyvsp[(7) - (8)].nd), (yyvsp[(8) - (8)].nd));
                    ;}
    break;

  case 505:

/* Line 1455 of yacc.c  */
#line 3198 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), 0, 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 506:

/* Line 1455 of yacc.c  */
#line 3202 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), 0, (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 507:

/* Line 1455 of yacc.c  */
#line 3206 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 508:

/* Line 1455 of yacc.c  */
#line 3210 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), 0, (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 509:

/* Line 1455 of yacc.c  */
#line 3214 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 0, 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 510:

/* Line 1455 of yacc.c  */
#line 3218 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 511:

/* Line 1455 of yacc.c  */
#line 3222 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    ;}
    break;

  case 512:

/* Line 1455 of yacc.c  */
#line 3226 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (2)].nd), 0, 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 513:

/* Line 1455 of yacc.c  */
#line 3230 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 514:

/* Line 1455 of yacc.c  */
#line 3234 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (2)].id), 0, (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 515:

/* Line 1455 of yacc.c  */
#line 3238 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (4)].id), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    ;}
    break;

  case 516:

/* Line 1455 of yacc.c  */
#line 3242 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 517:

/* Line 1455 of yacc.c  */
#line 3246 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, mrb_intern_lit(p->mrb, "&"));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    ;}
    break;

  case 518:

/* Line 1455 of yacc.c  */
#line 3253 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    ;}
    break;

  case 519:

/* Line 1455 of yacc.c  */
#line 3258 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    ;}
    break;

  case 520:

/* Line 1455 of yacc.c  */
#line 3263 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    ;}
    break;

  case 521:

/* Line 1455 of yacc.c  */
#line 3268 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    ;}
    break;

  case 522:

/* Line 1455 of yacc.c  */
#line 3275 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = 0;
                    ;}
    break;

  case 523:

/* Line 1455 of yacc.c  */
#line 3279 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (1)].id));
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    ;}
    break;

  case 524:

/* Line 1455 of yacc.c  */
#line 3286 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_arg(p, (yyvsp[(1) - (1)].id));
                    ;}
    break;

  case 525:

/* Line 1455 of yacc.c  */
#line 3290 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), 0);
                    ;}
    break;

  case 526:

/* Line 1455 of yacc.c  */
#line 3296 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 527:

/* Line 1455 of yacc.c  */
#line 3300 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 528:

/* Line 1455 of yacc.c  */
#line 3306 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (2)].id));
                      (yyval.id) = (yyvsp[(1) - (2)].id);
                    ;}
    break;

  case 529:

/* Line 1455 of yacc.c  */
#line 3313 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = cons(nsym((yyvsp[(1) - (2)].id)), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 530:

/* Line 1455 of yacc.c  */
#line 3320 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = cons(nsym((yyvsp[(1) - (2)].id)), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 531:

/* Line 1455 of yacc.c  */
#line 3327 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 532:

/* Line 1455 of yacc.c  */
#line 3331 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 533:

/* Line 1455 of yacc.c  */
#line 3337 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 534:

/* Line 1455 of yacc.c  */
#line 3341 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 537:

/* Line 1455 of yacc.c  */
#line 3351 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, (yyvsp[(2) - (2)].id));
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    ;}
    break;

  case 538:

/* Line 1455 of yacc.c  */
#line 3356 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      local_add_f(p, mrb_intern_lit(p->mrb, "*"));
                      (yyval.id) = -1;
                    ;}
    break;

  case 541:

/* Line 1455 of yacc.c  */
#line 3367 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    ;}
    break;

  case 542:

/* Line 1455 of yacc.c  */
#line 3373 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    ;}
    break;

  case 543:

/* Line 1455 of yacc.c  */
#line 3377 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.id) = 0;
                    ;}
    break;

  case 544:

/* Line 1455 of yacc.c  */
#line 3383 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    ;}
    break;

  case 545:

/* Line 1455 of yacc.c  */
#line 3387 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {p->lstate = EXPR_BEG;;}
    break;

  case 546:

/* Line 1455 of yacc.c  */
#line 3388 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      if ((yyvsp[(3) - (4)].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch ((enum node_type)intn((yyvsp[(3) - (4)].nd)->car)) {
                        case NODE_STR:
                        case NODE_DSTR:
                        case NODE_XSTR:
                        case NODE_DXSTR:
                        case NODE_DREGX:
                        case NODE_MATCH:
                        case NODE_FLOAT:
                        case NODE_ARRAY:
                        case NODE_HEREDOC:
                          yyerror(p, "can't define singleton method for literals");
                        default:
                          break;
                        }
                      }
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    ;}
    break;

  case 548:

/* Line 1455 of yacc.c  */
#line 3414 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    ;}
    break;

  case 549:

/* Line 1455 of yacc.c  */
#line 3420 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    ;}
    break;

  case 550:

/* Line 1455 of yacc.c  */
#line 3425 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 551:

/* Line 1455 of yacc.c  */
#line 3431 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(1) - (3)].nd));
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = cons((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 552:

/* Line 1455 of yacc.c  */
#line 3437 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[(1) - (3)].id)), (yyvsp[(3) - (3)].nd));
                    ;}
    break;

  case 553:

/* Line 1455 of yacc.c  */
#line 3442 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(3) - (3)].nd));
                      if ((yyvsp[(1) - (3)].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[(1) - (3)].nd)), (yyvsp[(3) - (3)].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[(1) - (3)].nd))), (yyvsp[(3) - (3)].nd));
                      }
                    ;}
    break;

  case 554:

/* Line 1455 of yacc.c  */
#line 3452 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      void_expr_error(p, (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = cons(cons((node*)NODE_KW_REST_ARGS, 0), (yyvsp[(2) - (2)].nd));
                    ;}
    break;

  case 567:

/* Line 1455 of yacc.c  */
#line 3479 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.num) = '.';
                    ;}
    break;

  case 568:

/* Line 1455 of yacc.c  */
#line 3483 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.num) = 0;
                    ;}
    break;

  case 570:

/* Line 1455 of yacc.c  */
#line 3490 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.num) = tCOLON2;
                    ;}
    break;

  case 580:

/* Line 1455 of yacc.c  */
#line 3514 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {yyerrok;;}
    break;

  case 583:

/* Line 1455 of yacc.c  */
#line 3520 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      p->lineno++;
                      p->column = 0;
                    ;}
    break;

  case 586:

/* Line 1455 of yacc.c  */
#line 3531 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"
    {
                      (yyval.nd) = 0;
                    ;}
    break;



/* Line 1455 of yacc.c  */
#line 9789 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-armeabi-v7a-neon-hard/mrbgems/mruby-compiler/core/y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (p, YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (p, yymsg);
	  }
	else
	  {
	    yyerror (p, YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, p);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (p, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, p);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 3535 "E:/UserData/Programming/Ruby/MRuby/fork/mruby/mrbgems/mruby-compiler/core/parse.y"

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, i);
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  yywarn(p, s);
}

static void
yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = intn(n->car);

  if (c == NODE_NTH_REF) {
    yyerror_i(p, "can't set variable $%" MRB_PRId, intn(n->cdr));
  }
  else if (c == NODE_BACK_REF) {
    yyerror_i(p, "can't set variable $%c", intn(n->cdr));
  }
  else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %S", mrb_fixnum_value(c));
  }
}

static void
void_expr_error(parser_state *p, node *n)
{
  int c;

  if (n == NULL) return;
  c = intn(n->car);
  switch (c) {
  case NODE_BREAK:
  case NODE_RETURN:
  case NODE_NEXT:
  case NODE_REDO:
  case NODE_RETRY:
    yyerror(p, "void value expression");
    break;
  case NODE_AND:
  case NODE_OR:
    void_expr_error(p, n->cdr->car);
    void_expr_error(p, n->cdr->cdr);
    break;
  case NODE_BEGIN:
    if (n->cdr) {
      while (n->cdr) {
        n = n->cdr;
      }
      void_expr_error(p, n->car);
    }
    break;
  default:
    break;
  }
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = intn(p->pb->car);
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else {
#ifndef MRB_DISABLE_STDIO
    if (p->f) {
      if (feof(p->f)) goto eof;
      c = fgetc(p->f);
      if (c == EOF) goto eof;
    }
    else
#endif
      if (!p->s || p->s >= p->send) {
        goto eof;
      }
      else {
        c = (unsigned char)*p->s++;
      }
  }
  if (c >= 0) {
    p->column++;
  }
  if (c == '\r') {
    c = nextc(p);
    if (c != '\n') {
      pushback(p, c);
      return '\r';
    }
    return c;
  }
  return c;

  eof:
  if (!p->cxt) return -1;
  else {
    if (p->cxt->partial_hook(p) < 0)
      return -1;                /* end of program(s) */
    return -2;                  /* end of a file in the program files */
  }
}

static void
pushback(parser_state *p, int c)
{
  if (c >= 0) {
    p->column--;
  }
  p->pb = cons(nint(c), p->pb);
}

static void
skip(parser_state *p, char term)
{
  int c;

  for (;;) {
    c = nextc(p);
    if (c < 0) break;
    if (c == term) break;
  }
}

static int
peekc_n(parser_state *p, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 == -1) return c0;    /* do not skip partial EOF */
    if (c0 >= 0) --p->column;
    list = push(list, nint(c0));
  } while(n--);
  if (p->pb) {
    p->pb = append((node*)list, p->pb);
  }
  else {
    p->pb = list;
  }
  return c0;
}

static mrb_bool
peek_n(parser_state *p, int c, int n)
{
  return peekc_n(p, n) == c && c >= 0;
}
#define peek(p,c) peek_n((p), (c), 0)

static mrb_bool
peeks(parser_state *p, const char *s)
{
  size_t len = strlen(s);

#ifndef MRB_DISABLE_STDIO
  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else
#endif
    if (p->s && p->s + len <= p->send) {
      if (memcmp(p->s, s, len) == 0) return TRUE;
    }
  return FALSE;
}

static mrb_bool
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    /* skip until first char */
    for (;;) {
      c = nextc(p);
      if (c < 0) return FALSE;
      if (c == '\n') {
        p->lineno++;
        p->column = 0;
      }
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      size_t len = strlen(s);

      while (len--) {
        if (nextc(p) == '\n') {
          p->lineno++;
          p->column = 0;
        }
      }
      return TRUE;
    }
    else{
      s--;
    }
  }
  return FALSE;
}


static int
newtok(parser_state *p)
{
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
    p->tokbuf = p->buf;
    p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  }
  p->tidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int32_t c)
{
  char utf8[4];
  int i, len;

  /* mrb_assert(-0x10FFFF <= c && c <= 0xFF); */
  if (c >= 0) {
    /* Single byte from source or non-Unicode escape */
    utf8[0] = (char)c;
    len = 1;
  }
  else {
    /* Unicode character */
    c = -c;
    if (c < 0x80) {
      utf8[0] = (char)c;
      len = 1;
    }
    else if (c < 0x800) {
      utf8[0] = (char)(0xC0 | (c >> 6));
      utf8[1] = (char)(0x80 | (c & 0x3F));
      len = 2;
    }
    else if (c < 0x10000) {
      utf8[0] = (char)(0xE0 |  (c >> 12)        );
      utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[2] = (char)(0x80 | ( c        & 0x3F));
      len = 3;
    }
    else {
      utf8[0] = (char)(0xF0 |  (c >> 18)        );
      utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
      utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[3] = (char)(0x80 | ( c        & 0x3F));
      len = 4;
    }
  }
  if (p->tidx+len >= p->tsiz) {
    if (p->tsiz >= MRB_PARSER_TOKBUF_MAX) {
      p->tidx += len;
      return;
    }
    p->tsiz *= 2;
    if (p->tokbuf == p->buf) {
      p->tokbuf = (char*)mrb_malloc(p->mrb, p->tsiz);
      memcpy(p->tokbuf, p->buf, MRB_PARSER_TOKBUF_SIZE);
    }
    else {
      p->tokbuf = (char*)mrb_realloc(p->mrb, p->tokbuf, p->tsiz);
    }
  }
  for (i = 0; i < len; i++) {
    p->tokbuf[p->tidx++] = utf8[i];
  }
}

static int
toklast(parser_state *p)
{
  return p->tokbuf[p->tidx-1];
}

static void
tokfix(parser_state *p)
{
  if (p->tidx >= MRB_PARSER_TOKBUF_MAX) {
    p->tidx = MRB_PARSER_TOKBUF_MAX-1;
    yyerror(p, "string too long (truncated)");
  }
  p->tokbuf[p->tidx] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->tokbuf;
}

static int
toklen(parser_state *p)
{
  return p->tidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static int32_t
scan_oct(const int *start, int len, int *retlen)
{
  const int *s = start;
  int32_t retval = 0;

  /* mrb_assert(len <= 3) */
  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = (int)(s - start);

  return retval;
}

static int32_t
scan_hex(parser_state *p, const int *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  const int *s = start;
  uint32_t retval = 0;
  char *tmp;

  /* mrb_assert(len <= 8) */
  while (len-- && *s && (tmp = (char*)strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = (int)(s - start);

  return (int32_t)retval;
}

static int32_t
read_escape_unicode(parser_state *p, int limit)
{
  int buf[9];
  int i;
  int32_t hex;

  /* Look for opening brace */
  i = 0;
  buf[0] = nextc(p);
  if (buf[0] < 0) {
  eof:
    yyerror(p, "invalid escape character syntax");
    return -1;
  }
  if (ISXDIGIT(buf[0])) {
    /* \uxxxx form */
    for (i=1; i<limit; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
  }
  else {
    pushback(p, buf[0]);
  }
  hex = scan_hex(p, buf, i, &i);
  if (i == 0 || hex > 0x10FFFF || (hex & 0xFFFFF800) == 0xD800) {
    yyerror(p, "invalid Unicode code point");
    return -1;
  }
  return hex;
}

/* Return negative to indicate Unicode code point */
static int32_t
read_escape(parser_state *p)
{
  int32_t c;

  switch (c = nextc(p)) {
  case '\\':/* Backslash */
    return c;

  case 'n':/* newline */
    return '\n';

  case 't':/* horizontal tab */
    return '\t';

  case 'r':/* carriage-return */
    return '\r';

  case 'f':/* form-feed */
    return '\f';

  case 'v':/* vertical tab */
    return '\13';

  case 'a':/* alarm(bell) */
    return '\007';

  case 'e':/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
  {
    int buf[3];
    int i;

    buf[0] = c;
    for (i=1; i<3; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (buf[i] < '0' || '7' < buf[i]) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_oct(buf, i, &i);
  }
  return c;

  case 'x':     /* hex constant */
  {
    int buf[2];
    int i;

    for (i=0; i<2; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
    if (i == 0) {
      yyerror(p, "invalid hex escape");
      return -1;
    }
    return scan_hex(p, buf, i, &i);
  }

  case 'u':     /* Unicode */
    if (peek(p, '{')) {
      /* \u{xxxxxxxx} form */
      nextc(p);
      c = read_escape_unicode(p, 8);
      if (c < 0) return 0;
      if (nextc(p) != '}') goto eof;
    }
    else {
      c = read_escape_unicode(p, 4);
      if (c < 0) return 0;
    }
  return -c;

  case 'b':/* backspace */
    return '\010';

  case 's':/* space */
    return ' ';

  case 'M':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
    if ((c = nextc(p)) == '\\') {
      return read_escape(p) | 0x80;
    }
    else if (c < 0) goto eof;
    else {
      return ((c & 0xff) | 0x80);
    }

  case 'C':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
  case 'c':
    if ((c = nextc(p))== '\\') {
      c = read_escape(p);
    }
    else if (c == '?')
      return 0177;
    else if (c < 0) goto eof;
    return c & 0x9f;

    eof:
  case -1:
  case -2:                      /* end of a file */
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = intn(p->lex_strterm->cdr->car);
  int beg = intn(p->lex_strterm->cdr->cdr->car);
  int end = intn(p->lex_strterm->cdr->cdr->cdr);
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;

  if (beg == 0) beg = -3;       /* should never happen */
  if (end == 0) end = -3;
  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
    if (hinf && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinf->line_head;
      hinf->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinf->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            ++s;
            --len;
          }
        }
        if ((len-1 == hinf->term_len) && (strncmp(s, hinf->term, len-1) == 0)) {
          if (c < 0) {
            p->parsing_heredoc = NULL;
          }
          else {
            return tHEREDOC_END;
          }
        }
      }
      if (c < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "can't find heredoc delimiter \"%s\" anywhere before EOF", hinf->term);
        yyerror(p, buf);
        return 0;
      }
      pylval.nd = new_str(p, tok(p), toklen(p));
      return tHD_STRING_MID;
    }
    if (c < 0) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->cdr->car = nint(nest_level);
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->cdr->car = nint(nest_level);
    }
    else if (c == '\\') {
      c = nextc(p);
      if (type & STR_FUNC_EXPAND) {
        if (c == end || c == beg) {
          tokadd(p, c);
        }
        else if (c == '\n') {
          p->lineno++;
          p->column = 0;
          if (type & STR_FUNC_ARRAY) {
            tokadd(p, '\n');
          }
        }
        else if (type & STR_FUNC_REGEXP) {
          tokadd(p, '\\');
          tokadd(p, c);
        }
        else if (c == 'u' && peek(p, '{')) {
          /* \u{xxxx xxxx xxxx} form */
          nextc(p);
          while (1) {
            do c = nextc(p); while (ISSPACE(c));
            if (c == '}') break;
            pushback(p, c);
            c = read_escape_unicode(p, 8);
            if (c < 0) break;
            tokadd(p, -c);
          }
          if (hinf)
            hinf->line_head = FALSE;
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinf)
            hinf->line_head = FALSE;
        }
      }
      else {
        if (c != beg && c != end) {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
          }
          if (!(c == '\\' || ((type & STR_FUNC_ARRAY) && ISSPACE(c)))) {
            tokadd(p, '\\');
          }
        }
        tokadd(p, c);
      }
      continue;
    }
    else if ((c == '#') && (type & STR_FUNC_EXPAND)) {
      c = nextc(p);
      if (c == '{') {
        tokfix(p);
        p->lstate = EXPR_BEG;
        p->cmd_start = TRUE;
        pylval.nd = new_str(p, tok(p), toklen(p));
        if (hinf) {
          hinf->line_head = FALSE;
          return tHD_STRING_PART;
        }
        return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    if ((type & STR_FUNC_ARRAY) && ISSPACE(c)) {
      if (toklen(p) == 0) {
        do {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
            heredoc_treat_nextline(p);
            if (p->parsing_heredoc != NULL) {
              return tHD_LITERAL_DELIM;
            }
          }
          c = nextc(p);
        } while (ISSPACE(c));
        pushback(p, c);
        return tLITERAL_DELIM;
      }
      else {
        pushback(p, c);
        tokfix(p);
        pylval.nd = new_str(p, tok(p), toklen(p));
        return tSTRING_MID;
      }
    }
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_ENDARG;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    pylval.nd = new_xstr(p, tok(p), toklen(p));
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    char *s = strndup(tok(p), toklen(p));
    char flags[3];
    char *flag = flags;
    char enc = '\0';
    char *encp;
    char *dup;

    newtok(p);
    while (re_opt = nextc(p), re_opt >= 0 && ISALPHA(re_opt)) {
      switch (re_opt) {
      case 'i': f |= 1; break;
      case 'x': f |= 2; break;
      case 'm': f |= 4; break;
      case 'u': f |= 16; break;
      case 'n': f |= 32; break;
      default: tokadd(p, re_opt); break;
      }
    }
    pushback(p, re_opt);
    if (toklen(p)) {
      char msg[128];
      tokfix(p);
      snprintf(msg, sizeof(msg), "unknown regexp option%s - %s",
          toklen(p) > 1 ? "s" : "", tok(p));
      yyerror(p, msg);
    }
    if (f != 0) {
      if (f & 1) *flag++ = 'i';
      if (f & 2) *flag++ = 'x';
      if (f & 4) *flag++ = 'm';
      if (f & 16) enc = 'u';
      if (f & 32) enc = 'n';
    }
    if (flag > flags) {
      dup = strndup(flags, (size_t)(flag - flags));
    }
    else {
      dup = NULL;
    }
    if (enc) {
      encp = strndup(&enc, 1);
    }
    else {
      encp = NULL;
    }
    pylval.nd = new_regx(p, s, dup, encp);

    return tREGEXP;
  }
  pylval.nd = new_str(p, tok(p), toklen(p));

  return tSTRING;
}


static int
heredoc_identifier(parser_state *p)
{
  int c;
  int type = str_heredoc;
  mrb_bool indent = FALSE;
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-') {
    indent = TRUE;
    c = nextc(p);
  }
  if (c == '\'' || c == '"') {
    int term = c;
    if (c == '\'')
      quote = TRUE;
    newtok(p);
    while ((c = nextc(p)) >= 0 && c != term) {
      if (c == '\n') {
        c = -1;
        break;
      }
      tokadd(p, c);
    }
    if (c < 0) {
      yyerror(p, "unterminated here document identifier");
      return 0;
    }
  }
  else {
    if (c < 0) {
      return 0;                 /* missing here document identifier */
    }
    if (! identchar(c)) {
      pushback(p, c);
      if (indent) pushback(p, '-');
      return 0;
    }
    newtok(p);
    do {
      tokadd(p, c);
    } while ((c = nextc(p)) >= 0 && identchar(c));
    pushback(p, c);
  }
  tokfix(p);
  newnode = new_heredoc(p);
  info = (parser_heredoc_info*)newnode->cdr;
  info->term = strndup(tok(p), toklen(p));
  info->term_len = toklen(p);
  if (! quote)
    type |= STR_FUNC_EXPAND;
  info->type = (string_type)type;
  info->allow_indent = indent;
  info->line_head = TRUE;
  info->doc = NULL;
  p->heredocs_from_nextline = push(p->heredocs_from_nextline, newnode);
  p->lstate = EXPR_END;

  pylval.nd = newnode;
  return tHEREDOC_BEG;
}

static int
arg_ambiguous(parser_state *p)
{
  yywarning(p, "ambiguous first argument; put parentheses or even spaces");
  return 1;
}

#include "lex.def"

static int
parser_yylex(parser_state *p)
{
  int32_t c;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->lex_strterm) {
    if (is_strterm_type(p, STR_FUNC_HEREDOC)) {
      if (p->parsing_heredoc != NULL)
        return parse_string(p);
    }
    else
      return parse_string(p);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
  retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\004':  /* ^D */
  case '\032':  /* ^Z */
  case '\0':    /* NUL */
  case -1:      /* end of script. */
    if (p->heredocs_from_nextline)
      goto maybe_heredoc;
    return 0;

  /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13':   /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':     /* it's a comment */
    skip(p, '\n');
    /* fall through */
  case -2:      /* end of a file */
  case '\n':
    maybe_heredoc:
    heredoc_treat_nextline(p);
  switch (p->lstate) {
  case EXPR_BEG:
  case EXPR_FNAME:
  case EXPR_DOT:
  case EXPR_CLASS:
  case EXPR_VALUE:
    p->lineno++;
    p->column = 0;
    if (p->parsing_heredoc != NULL) {
      if (p->lex_strterm) {
        return parse_string(p);
      }
    }
    goto retry;
  default:
    break;
  }
  if (p->parsing_heredoc != NULL) {
    return '\n';
  }
  while ((c = nextc(p))) {
    switch (c) {
    case ' ': case '\t': case '\f': case '\r':
    case '\13': /* '\v' */
      space_seen = 1;
      break;
    case '.':
      if ((c = nextc(p)) != '.') {
        pushback(p, c);
        pushback(p, '.');
        goto retry;
      }
    case -1:                  /* EOF */
    case -2:                  /* end of a file */
      goto normal_newline;
    default:
      pushback(p, c);
      goto normal_newline;
    }
  }
  normal_newline:
  p->cmd_start = TRUE;
  p->lstate = EXPR_BEG;
  return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("**",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "`**' interpreted as argument prefix");
        c = tDSTAR;
      }
      else if (IS_BEG()) {
        c = tDSTAR;
      }
      else {
        c = tPOW; /* "**", "argument prefix" */
      }
    }
    else {
      if (c == '=') {
        pylval.id = intern_c('*');
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'*' interpreted as argument prefix");
        c = tSTAR;
      }
      else if (IS_BEG()) {
        c = tSTAR;
      }
      else {
        c = '*';
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '!':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return '!';
      }
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if (c == '=') {
      return tNEQ;
    }
    if (c == '~') {
      return tNMATCH;
    }
    pushback(p, c);
    return '!';

  case '=':
    if (p->column == 1) {
      static const char begin[] = "begin";
      static const char end[] = "\n=end";
      if (peeks(p, begin)) {
        c = peekc_n(p, sizeof(begin)-1);
        if (c < 0 || ISSPACE(c)) {
          do {
            if (!skips(p, end)) {
              yyerror(p, "embedded document meets end of file");
              return 0;
            }
            c = nextc(p);
          } while (!(c < 0 || ISSPACE(c)));
          if (c != '\n') skip(p, '\n');
          p->lineno++;
          p->column = 0;
          goto retry;
        }
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      if ((c = nextc(p)) == '=') {
        return tEQQ;
      }
      pushback(p, c);
      return tEQ;
    }
    if (c == '~') {
      return tMATCH;
    }
    else if (c == '>') {
      return tASSOC;
    }
    pushback(p, c);
    return '=';

  case '<':
    c = nextc(p);
    if (c == '<' &&
        p->lstate != EXPR_DOT &&
        p->lstate != EXPR_CLASS &&
        !IS_END() &&
        (!IS_ARG() || space_seen)) {
      int token = heredoc_identifier(p);
      if (token)
        return token;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
      if (p->lstate == EXPR_CLASS) {
        p->cmd_start = TRUE;
      }
    }
    if (c == '=') {
      if ((c = nextc(p)) == '>') {
        return tCMP;
      }
      pushback(p, c);
      return tLEQ;
    }
    if (c == '<') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("<<",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern(">>",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->lex_strterm = new_strterm(p, str_dquote, '"', 0);
    return tSTRING_BEG;

  case '\'':
    p->lex_strterm = new_strterm(p, str_squote, '\'', 0);
    return parse_string(p);

  case '`':
    if (p->lstate == EXPR_FNAME) {
      p->lstate = EXPR_ENDFN;
      return '`';
    }
    if (p->lstate == EXPR_DOT) {
      if (cmd_state)
        p->lstate = EXPR_CMDARG;
      else
        p->lstate = EXPR_ARG;
      return '`';
    }
    p->lex_strterm = new_strterm(p, str_xquote, '`', 0);
    return tXSTRING_BEG;

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete character syntax");
      return 0;
    }
    if (ISSPACE(c)) {
      if (!IS_ARG()) {
        int c2;
        switch (c) {
        case ' ':
          c2 = 's';
          break;
        case '\n':
          c2 = 'n';
          break;
        case '\t':
          c2 = 't';
          break;
        case '\v':
          c2 = 'v';
          break;
        case '\r':
          c2 = 'r';
          break;
        case '\f':
          c2 = 'f';
          break;
        default:
          c2 = 0;
          break;
        }
        if (c2) {
          char buf[256];
          snprintf(buf, sizeof(buf), "invalid character syntax; use ?\\%c", c2);
          yyerror(p, buf);
        }
      }
      ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    newtok(p);
    /* need support UTF-8 if configured */
    if ((isalnum(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((isalnum(c2) || c2 == '_')) {
        goto ternary;
      }
    }
    if (c == '\\') {
      c = read_escape(p);
      tokadd(p, c);
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    pylval.nd = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_ENDARG;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("&&",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '.') {
      p->lstate = EXPR_DOT;
      return tANDDOT;
    }
    else if (c == '=') {
      pylval.id = intern_c('&');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "'&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("||",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = intern_c('|');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '|';

  case '+':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUPLUS;
      }
      pushback(p, c);
      return '+';
    }
    if (c == '=') {
      pylval.id = intern_c('+');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        c = '+';
        goto start_num;
      }
      return tUPLUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '+';

  case '-':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUMINUS;
      }
      pushback(p, c);
      return '-';
    }
    if (c == '=') {
      pylval.id = intern_c('-');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (c == '>') {
      p->lstate = EXPR_ENDFN;
      return tLAMBDA;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        return tUMINUS_NUM;
      }
      return tUMINUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '-';

  case '.':
    p->lstate = EXPR_BEG;
    if ((c = nextc(p)) == '.') {
      if ((c = nextc(p)) == '.') {
        return tDOT3;
      }
      pushback(p, c);
      return tDOT2;
    }
    pushback(p, c);
    if (c >= 0 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

    start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  {
    int is_float, seen_point, seen_e, nondigit;

    is_float = seen_point = seen_e = nondigit = 0;
    p->lstate = EXPR_ENDARG;
    newtok(p);
    if (c == '-' || c == '+') {
      tokadd(p, c);
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(p,"numeric literal without digits"); return 0;} while (0)
      int start = toklen(p);
      c = nextc(p);
      if (c == 'x' || c == 'X') {
        /* hexadecimal */
        c = nextc(p);
        if (c >= 0 && ISXDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISXDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, tolower(c));
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 16);
        return tINTEGER;
      }
      if (c == 'b' || c == 'B') {
        /* binary */
        c = nextc(p);
        if (c == '0' || c == '1') {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (c != '0' && c != '1') break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 2);
        return tINTEGER;
      }
      if (c == 'd' || c == 'D') {
        /* decimal */
        c = nextc(p);
        if (c >= 0 && ISDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 10);
        return tINTEGER;
      }
      if (c == '_') {
        /* 0_0 */
        goto octal_number;
      }
      if (c == 'o' || c == 'O') {
        /* prefixed octal */
        c = nextc(p);
        if (c < 0 || c == '_' || !ISDIGIT(c)) {
          no_digits();
        }
      }
      if (c >= '0' && c <= '7') {
        /* octal */
        octal_number:
        do {
          if (c == '_') {
            if (nondigit) break;
            nondigit = c;
            continue;
          }
          if (c < '0' || c > '9') break;
          if (c > '7') goto invalid_octal;
          nondigit = 0;
          tokadd(p, c);
        } while ((c = nextc(p)) >= 0);

        if (toklen(p) > start) {
          pushback(p, c);
          tokfix(p);
          if (nondigit) goto trailing_uc;
          pylval.nd = new_int(p, tok(p), 8);
          return tINTEGER;
        }
        if (nondigit) {
          pushback(p, c);
          goto trailing_uc;
        }
      }
      if (c > '7' && c <= '9') {
        invalid_octal:
        yyerror(p, "Invalid octal digit");
      }
      else if (c == '.' || c == 'e' || c == 'E') {
        tokadd(p, '0');
      }
      else {
        pushback(p, c);
        pylval.nd = new_int(p, "0", 10);
        return tINTEGER;
      }
    }

    for (;;) {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        nondigit = 0;
        tokadd(p, c);
        break;

      case '.':
        if (nondigit) goto trailing_uc;
        if (seen_point || seen_e) {
          goto decode_num;
        }
        else {
          int c0 = nextc(p);
          if (c0 < 0 || !ISDIGIT(c0)) {
            pushback(p, c0);
            goto decode_num;
          }
          c = c0;
        }
        tokadd(p, '.');
        tokadd(p, c);
        is_float++;
        seen_point++;
        nondigit = 0;
        break;

      case 'e':
      case 'E':
        if (nondigit) {
          pushback(p, c);
          c = nondigit;
          goto decode_num;
        }
        if (seen_e) {
          goto decode_num;
        }
        tokadd(p, c);
        seen_e++;
        is_float++;
        nondigit = c;
        c = nextc(p);
        if (c != '-' && c != '+') continue;
        tokadd(p, c);
        nondigit = c;
        break;

      case '_':       /* '_' in number just ignored */
        if (nondigit) goto decode_num;
        nondigit = c;
        break;

      default:
        goto decode_num;
      }
      c = nextc(p);
    }

    decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
      yyerror_i(p, "trailing '%c' in number", nondigit);
    }
    tokfix(p);
    if (is_float) {
#ifdef MRB_WITHOUT_FLOAT
      yywarning_s(p, "floating point numbers are not supported", tok(p));
      pylval.nd = new_int(p, "0", 10);
      return tINTEGER;
#else
      double d;
      char *endp;

      errno = 0;
      d = mrb_float_read(tok(p), &endp);
      if (d == 0 && endp == tok(p)) {
        yywarning_s(p, "corrupted float value %s", tok(p));
      }
      else if (errno == ERANGE) {
        yywarning_s(p, "float %s out of range", tok(p));
        errno = 0;
      }
      pylval.nd = new_float(p, tok(p));
      return tFLOAT;
#endif
    }
    pylval.nd = new_int(p, tok(p), 10);
    return tINTEGER;
  }

  case ')':
  case ']':
    p->paren_nest--;
    /* fall through */
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_END;
    return c;

  case ':':
    c = nextc(p);
    if (c == ':') {
      if (IS_BEG() || p->lstate == EXPR_CLASS || IS_SPCARG(-1)) {
        p->lstate = EXPR_BEG;
        return tCOLON3;
      }
      p->lstate = EXPR_DOT;
      return tCOLON2;
    }
    if (!space_seen && IS_END()) {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return tLABEL_TAG;
    }
    if (!ISSPACE(c) || IS_BEG()) {
      pushback(p, c);
      p->lstate = EXPR_FNAME;
      return tSYMBEG;
    }
    pushback(p, c);
    p->lstate = EXPR_BEG;
    return ':';

  case '/':
    if (IS_BEG()) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('/');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('^');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '^';

  case ';':
    p->lstate = EXPR_BEG;
    return ';';

  case ',':
    p->lstate = EXPR_BEG;
    return ',';

  case '~':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      if ((c = nextc(p)) != '@') {
        pushback(p, c);
      }
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '~';

  case '(':
    if (IS_BEG()) {
      c = tLPAREN;
    }
    else if (IS_SPCARG(-1)) {
      c = tLPAREN_ARG;
    }
    else if (p->lstate == EXPR_END && space_seen) {
      c = tLPAREN_ARG;
    }
    p->paren_nest++;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '[':
    p->paren_nest++;
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if ((c = nextc(p)) == ']') {
        if ((c = nextc(p)) == '=') {
          return tASET;
        }
        pushback(p, c);
        return tAREF;
      }
      pushback(p, c);
      return '[';
    }
    else if (IS_BEG()) {
      c = tLBRACK;
    }
    else if (IS_ARG() && space_seen) {
      c = tLBRACK;
    }
    p->lstate = EXPR_BEG;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    return c;

  case '{':
    if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
      p->lstate = EXPR_BEG;
      p->lpar_beg = 0;
      p->paren_nest--;
      COND_PUSH(0);
      CMDARG_PUSH(0);
      return tLAMBEG;
    }
    if (IS_ARG() || p->lstate == EXPR_END || p->lstate == EXPR_ENDFN)
      c = '{';          /* block (primary) */
    else if (p->lstate == EXPR_ENDARG)
      c = tLBRACE_ARG;  /* block (expr) */
    else
      c = tLBRACE;      /* hash */
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '\\':
    c = nextc(p);
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
      int paren;

      c = nextc(p);
      quotation:
      if (c < 0 || !ISALNUM(c)) {
        term = c;
        c = 'Q';
      }
      else {
        term = nextc(p);
        if (isalnum(term)) {
          yyerror(p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(p, "unterminated quoted string meets end of file");
        return 0;
      }
      paren = term;
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      else paren = 0;

      switch (c) {
      case 'Q':
        p->lex_strterm = new_strterm(p, str_dquote, term, paren);
        return tSTRING_BEG;

      case 'q':
        p->lex_strterm = new_strterm(p, str_squote, term, paren);
        return parse_string(p);

      case 'W':
        p->lex_strterm = new_strterm(p, str_dword, term, paren);
        return tWORDS_BEG;

      case 'w':
        p->lex_strterm = new_strterm(p, str_sword, term, paren);
        return tWORDS_BEG;

      case 'x':
        p->lex_strterm = new_strterm(p, str_xquote, term, paren);
        return tXSTRING_BEG;

      case 'r':
        p->lex_strterm = new_strterm(p, str_regexp, term, paren);
        return tREGEXP_BEG;

      case 's':
        p->lex_strterm = new_strterm(p, str_ssym, term, paren);
        return tSYMBEG;

      case 'I':
        p->lex_strterm = new_strterm(p, str_dsymbols, term, paren);
        return tSYMBOLS_BEG;

      case 'i':
        p->lex_strterm = new_strterm(p, str_ssymbols, term, paren);
        return tSYMBOLS_BEG;

      default:
        yyerror(p, "unknown type of %string");
        return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('%');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete global variable syntax");
      return 0;
    }
    switch (c) {
    case '_':     /* $_: last read line string */
      c = nextc(p);
      if (c >= 0 && identchar(c)) { /* if there is more after _ it is a variable */
        tokadd(p, '$');
        tokadd(p, c);
        break;
      }
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':     /* $~: match-data */
    case '*':     /* $*: argv */
    case '$':     /* $$: pid */
    case '?':     /* $?: last status */
    case '!':     /* $!: error string */
    case '@':     /* $@: error position */
    case '/':     /* $/: input record separator */
    case '\\':    /* $\: output record separator */
    case ';':     /* $;: field separator */
    case ',':     /* $,: output field separator */
    case '.':     /* $.: last read line number */
    case '=':     /* $=: ignorecase */
    case ':':     /* $:: load path */
    case '<':     /* $<: reading filename */
    case '>':     /* $>: default output handle */
    case '\"':    /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
      gvar:
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '&':     /* $&: last match */
    case '`':     /* $`: string before last match */
    case '\'':    /* $': string after last match */
    case '+':     /* $+: string matches last pattern */
      if (last_state == EXPR_FNAME) {
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;
      }
      pylval.nd = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      do {
        tokadd(p, c);
        c = nextc(p);
      } while (c >= 0 && isdigit(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      {
        unsigned long n = strtoul(tok(p), NULL, 10);
        if (n > INT_MAX) {
          yyerror_i(p, "capture group index must be <= %d", INT_MAX);
          return 0;
        }
        pylval.nd = new_nth_ref(p, (int)n);
      }
      return tNTH_REF;

    default:
      if (!identchar(c)) {
        pushback(p,  c);
        return '$';
      }
      /* fall through */
    case '0':
      tokadd(p, '$');
    }
    break;

    case '@':
      c = nextc(p);
      token_column = newtok(p);
      tokadd(p, '@');
      if (c == '@') {
        tokadd(p, '@');
        c = nextc(p);
      }
      if (c < 0) {
        if (p->tidx == 1) {
          yyerror(p, "incomplete instance variable syntax");
        }
        else {
          yyerror(p, "incomplete class variable syntax");
        }
        return 0;
      }
      else if (isdigit(c)) {
        if (p->tidx == 1) {
          yyerror_i(p, "'@%c' is not allowed as an instance variable name", c);
        }
        else {
          yyerror_i(p, "'@@%c' is not allowed as a class variable name", c);
        }
        return 0;
      }
      if (!identchar(c)) {
        pushback(p, c);
        return '@';
      }
      break;

    case '_':
      token_column = newtok(p);
      break;

    default:
      if (!identchar(c)) {
        yyerror_i(p,  "Invalid char '\\x%02X' in expression", c);
        goto retry;
      }

      token_column = newtok(p);
      break;
  }

  do {
    tokadd(p, c);
    c = nextc(p);
    if (c < 0) break;
  } while (identchar(c));
  if (token_column == 0 && toklen(p) == 7 && (c < 0 || c == '\n') &&
      strncmp(tok(p), "__END__", toklen(p)) == 0)
    return -1;

  switch (tok(p)[0]) {
  case '@': case '$':
    pushback(p, c);
    break;
  default:
    if ((c == '!' || c == '?') && !peek(p, '=')) {
      tokadd(p, c);
    }
    else {
      pushback(p, c);
    }
  }
  tokfix(p);
  {
    int result = 0;

    switch (tok(p)[0]) {
    case '$':
      p->lstate = EXPR_END;
      result = tGVAR;
      break;
    case '@':
      p->lstate = EXPR_END;
      if (tok(p)[1] == '@')
        result = tCVAR;
      else
        result = tIVAR;
      break;

    default:
      if (toklast(p) == '!' || toklast(p) == '?') {
        result = tFID;
      }
      else {
        if (p->lstate == EXPR_FNAME) {
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
        }
        if (result == 0 && ISUPPER(tok(p)[0])) {
          result = tCONSTANT;
        }
        else {
          result = tIDENTIFIER;
        }
      }

      if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
          p->lstate = EXPR_END;
          tokfix(p);
          pylval.id = intern_cstr(tok(p));
          return tIDENTIFIER;
        }
      }
      if (p->lstate != EXPR_DOT) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = mrb_reserved_word(tok(p), toklen(p));
        if (kw) {
          enum mrb_lex_state_enum state = p->lstate;
          pylval.num = p->lineno;
          p->lstate = kw->state;
          if (state == EXPR_FNAME) {
            pylval.id = intern_cstr(kw->name);
            return kw->id[0];
          }
          if (p->lstate == EXPR_BEG) {
            p->cmd_start = TRUE;
          }
          if (kw->id[0] == keyword_do) {
            if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
              p->lpar_beg = 0;
              p->paren_nest--;
              return keyword_do_LAMBDA;
            }
            if (COND_P()) return keyword_do_cond;
            if (CMDARG_P() && state != EXPR_CMDARG)
              return keyword_do_block;
            if (state == EXPR_ENDARG || state == EXPR_BEG)
              return keyword_do_block;
            return keyword_do;
          }
          if (state == EXPR_BEG || state == EXPR_VALUE)
            return kw->id[0];
          else {
            if (kw->id[0] != kw->id[1])
              p->lstate = EXPR_BEG;
            return kw->id[1];
          }
        }
      }

      if (IS_BEG() || p->lstate == EXPR_DOT || IS_ARG()) {
        if (cmd_state) {
          p->lstate = EXPR_CMDARG;
        }
        else {
          p->lstate = EXPR_ARG;
        }
      }
      else if (p->lstate == EXPR_FNAME) {
        p->lstate = EXPR_ENDFN;
      }
      else {
        p->lstate = EXPR_END;
      }
    }
    {
      mrb_sym ident = intern_cstr(tok(p));

      pylval.id = ident;
      if (last_state != EXPR_DOT && islower(tok(p)[0]) && local_var_p(p, ident)) {
        p->lstate = EXPR_END;
      }
    }
    return result;
  }
}

static int
yylex(void *lval, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
{
  if (!cxt) return;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
  p->no_optimize = cxt->no_optimize;
  p->on_eval = cxt->on_eval;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrbc_context *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if (intn(p->tree->car) != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym *)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_codedump_all(mrb_state*, struct RProc*);
void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  struct mrb_jmpbuf buf1;
  p->jmp = &buf1;

  MRB_TRY(p->jmp) {
    int n = 1;

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;

    parser_init_cxt(p, c);

    if (p->mrb->jmp) {
      n = yyparse(p);
    }
    else {
      struct mrb_jmpbuf buf2;

      p->mrb->jmp = &buf2;
      MRB_TRY(p->mrb->jmp) {
        n = yyparse(p);
      }
      MRB_CATCH(p->mrb->jmp) {
        p->nerr++;
      }
      MRB_END_EXC(p->mrb->jmp);
      p->mrb->jmp = 0;
    }
    if (n != 0 || p->nerr > 0) {
      p->tree = 0;
      return;
    }
    if (!p->tree) {
      p->tree = new_nil(p);
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      mrb_parser_dump(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->jmp) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }
  MRB_END_EXC(p->jmp);
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return NULL;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return NULL;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;

  p->s = p->send = NULL;
#ifndef MRB_DISABLE_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = 0;

  p->capture_errors = FALSE;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif
  p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  p->tokbuf = p->buf;

  p->lex_strterm = NULL;
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
  }
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
}

MRB_API void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->filename);
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char *)mrb_malloc(mrb, len + 1);

    memcpy(p, s, len + 1);
    if (c->filename) {
      mrb_free(mrb, c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename = mrb_sym2name_len(p->mrb, sym, NULL);
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = (int)i;
      return;
    }
  }

  p->current_filename_index = (int)p->filename_table_length++;

  new_table = (mrb_sym*)parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memmove(new_table, p->filename_table, sizeof(mrb_sym) * p->current_filename_index);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

MRB_API char const*
mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) { return NULL; }
  else {
    return mrb_sym2name_len(p->mrb, p->filename_table[idx], NULL);
  }
}

#ifndef MRB_DISABLE_STDIO
MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrbc_context *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_value v;
  unsigned int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (c) c->parser_nerr = p->nerr;
    if (p->capture_errors) {
      char buf[256];
      int n;

      n = snprintf(buf, sizeof(buf), "line %d: %s\n",
          p->error_buffer[0].lineno, p->error_buffer[0].message);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      if (mrb->exc == NULL) {
        mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      }
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    }
    return mrb_undef_value();
  }
  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) {
      target = c->target_class;
    }
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  MRB_PROC_SET_TARGET_CLASS(proc, target);
  if (mrb->c->ci) {
    mrb->c->ci->target_class = target;
  }
  v = mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifndef MRB_DISABLE_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifndef MRB_DISABLE_STDIO

static void
dump_prefix(node *tree, int offset)
{
  printf("%05d ", tree->lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

static void
dump_args(mrb_state *mrb, node *n, int offset)
{
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("mandatory args:\n");
    dump_recur(mrb, n->car, offset+2);
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("optional args:\n");
    {
      node *n2 = n->car;

      while (n2) {
        dump_prefix(n2, offset+2);
        printf("%s=\n", mrb_sym2name(mrb, sym(n2->car->car)));
        mrb_parser_dump(mrb, n2->car->cdr, offset+3);
        n2 = n2->cdr;
      }
    }
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("post mandatory args:\n");
    dump_recur(mrb, n->car, offset+2);
  }

  n = n->cdr;
  if (n) {
    mrb_assert(intn(n->car) == NODE_ARGS_TAIL);
    mrb_parser_dump(mrb, n, offset);
  }
}

#endif

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifndef MRB_DISABLE_STDIO
  int nodetype;

  if (!tree) return;
  again:
  dump_prefix(tree, offset);
  nodetype = intn(tree->car);
  tree = tree->cdr;
  switch (nodetype) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(n2, offset+1);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(n2, offset+2);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_LAMBDA:\n");
    dump_prefix(tree, offset);
    goto block;

  case NODE_BLOCK:
    block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset+1);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(tree, offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(n2, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
  case NODE_SCALL:
    switch (nodetype) {
    case NODE_FCALL:
      printf("NODE_FCALL:\n"); break;
    case NODE_CALL:
      printf("NODE_CALL(.):\n"); break;
    case NODE_SCALL:
      printf("NODE_SCALL(&.):\n"); break;
    default:
      break;
    }
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("method='%s' (%d)\n",
        mrb_sym2name(mrb, sym(tree->cdr->car)),
        intn(tree->cdr->car));
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3: ::%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_KW_HASH:
    printf("NODE_KW_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(tree, offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(tree, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          if (n2->car == (node*)-1) {
            dump_prefix(n2, offset+2);
            printf("(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), intn(tree->car));
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(tree, offset + 1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(tree, offset + 1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", intn(tree));
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%d\n", intn(tree));
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, intn(tree->cdr->car));
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR \"%s\" len %d\n", (char*)tree->car, intn(tree->cdr));
    break;

  case NODE_DSTR:
    printf("NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR \"%s\" len %d\n", (char*)tree->car, intn(tree->cdr));
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    if (tree->cdr->cdr->cdr->car) {
      dump_prefix(tree, offset);
      printf("opt: %s\n", (char*)tree->cdr->cdr->cdr->car);
    }
    if (tree->cdr->cdr->cdr->cdr) {
      dump_prefix(tree, offset);
      printf("enc: %s\n", (char*)tree->cdr->cdr->cdr->cdr);
    }
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s (%d)\n", mrb_sym2name(mrb, sym(tree)),
           intn(tree));
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
        mrb_sym2name(mrb, sym(tree->car)),
        mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym2name(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(tree, offset+1);
    printf("%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset);
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf(":%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC (<<%s):\n", ((parser_heredoc_info*)tree)->term);
    dump_recur(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  case NODE_ARGS_TAIL:
    printf("NODE_ARGS_TAIL:\n");
    {
      node *kws = tree->car;

      while (kws) {
        mrb_parser_dump(mrb, kws->car, offset+1);
        kws = kws->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      mrb_assert(intn(tree->car->car) == NODE_KW_REST_ARGS);
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("block='%s'\n", mrb_sym2name(mrb, sym(tree->car)));
    }
    break;

  case NODE_KW_ARG:
    printf("NODE_KW_ARG %s\n", mrb_sym2name(mrb, sym(tree->car)));
    mrb_parser_dump(mrb, tree->cdr->car, offset + 1);
    break;

  case NODE_KW_REST_ARGS:
    printf("NODE_KW_REST_ARGS %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  default:
    printf("node type: %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}

