      implicit none

      character*70 finp, fout

      integer natom,nbond


      write(6,*)'natom='
      read(5,*)natom

      write(6,*)'input file name'
      read(5,*)finp

      write(6,*)'output file name'
      read(5,*)fout

      write(6,*)'number of bonds'
      read(5,*)nbond
     
      open(unit=10,file=finp,status='old')

      open(unit=11,file=fout,status='new')

      call topol(natom,nbond,finp)


      stop
      end

      subroutine topol(natom,nbond,finp)

      integer natom,nbond
      integer iatom,ibond
      integer jatom
      integer i,j,k,l
      integer ka,ki
      integer katom1,katom2
      integer bondlist(4)
      integer ibondlist,jbondlist
      integer dlist1(3),dlist2(3)
      integer idlist1,idlist2,kdlis1,kdlist2

      character*70 shosiki1,shosiki2
      character*70 finp
      logical bmat(natom,natom)


      shosiki1='(2i5)'

      bmat=.false.

      do ibond=1,nbond
        read(10,shosiki1)ka,ki
        bmat(ka,ki)=.true.
        bmat(ki,ka)=.true.
      enddo

      close(10)
      
      do iatom=1,natom
        ka=0
        bondlist=0
        ki=1
        do jatom=1,natom
          if(bmat(iatom,jatom))then
            bondlist(ki)=jatom
            ki=ki+1
            ka=ka+1
          endif    
        enddo
        if(ka.ge.2)then
        do ibondlist=1,ka-1
          do jbondlist=ibondlist+1,ka
            write(11,'(3i5)')bondlist(ibondlist),iatom,
     $         bondlist(jbondlist)
          enddo
        enddo
        endif
      enddo

      open(unit=10,file=finp,status='old')
     
      do ibond=1,nbond
        read(10,'(2i5)')katom1,katom2
        dlist1=0
        dlist2=0
        kdlist1=0
        kdlist2=0
!      get the list of atoms which is connected to the 1st atom of the pair
        do iatom=1,natom
          if(iatom.eq.katom2)go to 100
          if(bmat(katom1,iatom))then
            
            kdlist1=kdlist1+1
            dlist1(kdlist1)=iatom
          endif
  100     continue        
        enddo
       
!     get the list of atoms which is connected to katom2

        do iatom=1,natom
          if(iatom.eq.katom1)go to 101
            if(bmat(katom2,iatom))then
              kdlist2=kdlist2+1
              dlist2(kdlist2)=iatom
            endif

 101      continue
        enddo    

        do idlist1=1,kdlist1
           ka=dlist1(idlist1)
           do idlist2=1,kdlist2
             ki=dlist2(idlist2)
        write(11,'(4i5)')ka,katom1,katom2,ki
           enddo
         enddo
      enddo

         
      return
      end

