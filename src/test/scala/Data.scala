package hayago.game.tests

import hayago.game._
import scala.collection.immutable.HashSet

object Data {

  trait TestGame {
    val completeSession: Session
    val expectedDeadStones: HashSet[Intersection]
    val expectedScoreBlack: Score
    val expectedScoreWhite: Score
    val description: String
  }

  object second_hayago_championship extends TestGame {
    private val sgf = "(;\nEV[2nd Hayago Championship]\nRO[Final]\nPB[Hashimoto Utaro]\nBR[9p]\nPW[Ohira Shuzo]\nWR[9p]\nKM[5.5]\nRE[B+0.5]\nDT[1970]\nGC[Broadcast 1970-03-22,29]\n\n;B[qd];W[dd];B[pq];W[oc];B[cp];W[po];B[ep];W[oq];B[or];W[op]\n;B[nq];W[nr];B[mr];W[pr];B[ns];W[qq];B[mc];W[oe];B[qg];W[qc]\n;B[rc];W[qb];B[pd];W[od];B[me];W[nf];B[pc];W[pb];B[nb];W[ob]\n;B[re];W[ld];B[md];W[lg];B[jc];W[mp];B[kq];W[qi];B[pi];W[pj]\n;B[ph];W[nh];B[qj];W[qk];B[rj];W[rk];B[oj];W[pk];B[cj];W[hc]\n;B[ri];W[kb];B[kc];W[jb];B[lb];W[ic];B[je];W[oi];B[ko];W[dq]\n;B[eq];W[cg];B[lf];W[kg];B[if];W[ih];B[mg];W[mf];B[kf];W[jg]\n;B[cc];W[dc];B[cd];W[cb];B[cf];W[df];B[ce];W[dg];B[bb];W[db]\n;B[bg];W[bh];B[af];W[ci];B[fd];W[ba];B[ab];W[ge];B[gd];W[he]\n;B[de];W[ee];B[ka];W[ib];B[ed];W[fe];B[na];W[km];B[cl];W[in]\n;B[io];W[dp];B[do];W[cq];B[bp];W[er];B[fr];W[cr];B[ds];W[co]\n;B[bo];W[cn];B[dn];W[bq];B[bn];W[es];B[nj];W[mi];B[om];W[nl]\n;B[nm];W[qm];B[mn];W[ml];B[np];W[no];B[mo];W[fq];B[kk];W[mj]\n;B[gr];W[ho];B[jn];W[ip];B[gp];W[fp];B[eo];W[fo];B[go];W[en]\n;B[dm];W[gn];B[gq];W[fn];B[jo];W[jm];B[im];W[hn];B[il];W[gk]\n;B[hp];W[kl];B[gm];W[ad];B[bd];W[ah];B[bf];W[mq];B[nr];W[ae]\n;B[ac];W[ik];B[hm];W[hk];B[hg];W[fl];B[fm];W[rh];B[qh];W[si]\n;B[sj];W[sk];B[sh];W[bj];B[og];W[ng];B[hh];W[hi];B[ar];W[aq]\n;B[bs];W[cs];B[ps];W[qr];B[pn];W[qn];B[fs];W[dr];B[ck];W[rb]\n;B[gi];W[el];B[em];W[bk];B[di];W[bl];B[dh];W[ch];B[hj];W[ii]\n;B[gj];W[ij];B[of];W[pe];B[ne];W[rd];B[sd];W[qe];B[rd];W[rf]\n;B[qf];W[sb];B[rg];W[oa];B[ni];W[ie];B[jf];W[bm];B[cm];W[br]\n;B[fg];W[sf];B[sg];W[ja];B[la];W[ej];B[jl];W[jk];B[oh];W[mh]\n;B[ca];W[fb];B[am];W[al];B[eg];W[fi];B[gh];W[dj];B[eh];W[fj]\n;B[pl];W[ql];B[mm];W[ll];B[nk];W[ol];B[mk];W[lk];B[pm];W[ln]\n;B[ap];W[as];B[lp];W[lo];B[qs];W[rs];B[os];W[rq];B[an];W[jd]\n;B[kd];W[id];B[aa];W[dk];B[gl];W[pf];B[ef];W[da];B[be];W[ba]\n;B[hf];W[gf];B[ca];W[ok];B[oi];W[ba];B[rr];W[sr];B[ca]\nC[B connects the ko]\n)"
    val completeSession = hayago.sgf.parse (sgf)
    val expectedDeadStones = HashSet () ++ List ("E:4", "F:4", "G:4", "I:16", "K:11", "L:4", "M:16", "M:17", "P:17", "R:6", "S:6").map (Intersection.unapply(_).get)
    val expectedScoreBlack = Score (0f, 49, 22, 6)
    val expectedScoreWhite = Score (5.5f, 56, 7, 5)
    val description = "the second Hayago championship"
  }

  object example9x9 extends TestGame {
    private val sgf = "(;\nCA[shift_jis]\nSZ[9]\nAP[MultiGo:4.4.4]\nGN[ﾚｰﾃｨﾝｸﾞ対局]\nDT[20130512 [21:49]]PB[村川大介 9段P]\nPW[井山裕太 9段P]\nKM[6.5]\nHA[0]\nRE[白1.5目勝ち!]\nUS[www.cyberoro.com]\nMULTIGOGM[1]\n;B[fd];W[cf];B[eg];W[dd];B[dc];W[cc];B[de];W[cd];B[ed];W[he]\n;B[ce];W[be];B[df];W[bf];B[hd];W[ge];B[gd];W[gg];B[db];W[cb]\n;B[cg];W[bg];B[gh];W[fh];B[hh];W[fg];B[eh];W[ei];B[di];W[fi]\n;B[hg];W[dh];B[ch];W[ci];B[bh];W[ff];B[fe];W[hf];B[id];W[bi]\n;B[ah];W[ef];B[dg];W[ee];B[di];W[ig];B[ai];W[ih];B[fb];W[hi]\n;B[ag];W[ab];B[bd];W[bc];B[ae];W[ad];B[af];W[bd];B[ca];W[ba]\n;B[da];W[ie]\n)\n    "
    val completeSession = hayago.sgf.parse (sgf)
    val expectedDeadStones = HashSet () ++ List ("G:8", "H:8", "H:7").map (Intersection.unapply(_).get)
    val expectedScoreBlack = Score (0f, 17, 3, 0)
    val expectedScoreWhite = Score (6.5f, 10, 2, 3)
    val description = "an example pro 9x9 game"
  }
}
